package controllers

import anorm.SqlParser.{bool, get, int, long, str}
import anorm._
import com.aha00a.commons.Implicits._
import com.aha00a.play.AnormSqlParser.localDateTime
import io.circe.generic.auto._
import io.circe.syntax._
import models.tables.Site
import play.api.Logging
import play.api.db.Database
import play.api.mvc._

import java.sql.Connection
import java.time.LocalDateTime
import java.time.YearMonth
import java.time.format.DateTimeFormatter
import javax.inject._

/**
 * What the admin screens report on: users, page views, daily counts, recent changes, site
 * activity, and the access log.
 *
 * These are read-only queries over the whole instance, which is why they are apart from the
 * endpoints that change a site. They are also the endpoints most likely to be slow, and
 * keeping them together makes that visible in one file rather than scattered through the
 * site-administration code.
 */
class ApiAdminReport @Inject()(
  implicit val
  controllerComponents: ControllerComponents,
  database: Database,
) extends BaseController with JsonResults with AdminAuth with Logging {

  /**
   * Whether the sites are being used: new and active sites per month, and what each site did
   * in the last 30 days. The numbers behind the measurement section of the wiki's
   * ToDo-User-Acquisition page, which asks to look at them monthly on one fixed definition.
   */
  def adminSiteActivity: Action[AnyContent] = Action { implicit request =>
    if (!isAdmin) {
      AccessDenied
    } else {
      database.withConnection { implicit connection =>
        Ok(ApiAdminReport.siteActivity(LocalDateTime.now()).asJson)
      }
    }
  }

  def adminUsers: Action[AnyContent] = Action { implicit request =>
    if (!isAdmin) {
      AccessDenied
    } else {
      database.withConnection { implicit connection =>
        case class AdminUser(
          seq: Long,
          created: String,
          updated: String,
          email: String,
          nickname: String,
          profileImageUrl: Option[String],
          visitCount: Long,
          lastViewed: Option[String],
        )

        val page = request.getQueryString("page")
          .flatMap(raw => scala.util.Try(raw.toInt).toOption)
          .map(_.max(1))
          .getOrElse(1)
        val pageSize = request.getQueryString("pageSize")
          .flatMap(raw => scala.util.Try(raw.toInt).toOption)
          .map(_.max(1).min(1000))
          .getOrElse(20)
        val search = request.getQueryString("search").map(_.trim).getOrElse("")
        val sortByRaw = request.getQueryString("sortBy").map(_.trim).getOrElse("seq")
        val sortOrderRaw = request.getQueryString("sortOrder").map(_.trim.toLowerCase).getOrElse("desc")

        val sortBy = sortByRaw match {
          case "created" => "U.created"
          case "updated" => "U.updated"
          case "email" => "email"
          case "nickname" => "U.nickname"
          case "visitCount" => "visit_count"
          case "lastViewed" => "last_viewed_raw"
          case _ => "U.seq"
        }
        val sortOrder = if (sortOrderRaw == "asc") "ASC" else "DESC"
        val offset = (page - 1) * pageSize
        val searchLike = s"%$search%"

        val count = SQL"""
          SELECT COUNT(*) AS count_value
          FROM User U
          LEFT JOIN (
            SELECT
              user,
              GROUP_CONCAT(email ORDER BY isPrimary DESC, email SEPARATOR ', ') AS emails
            FROM UserEmail
            GROUP BY user
          ) UE ON UE.user = U.seq
          WHERE (
            ${search.isEmpty} = TRUE OR
            UE.emails LIKE $searchLike OR
            U.nickname LIKE $searchLike OR
            CAST(U.seq AS CHAR) LIKE $searchLike
          )
        """.as(long("count_value").single)

        val orderBySql = s"$sortBy $sortOrder"
        val users = SQL(s"""
          SELECT
            U.seq,
            DATE_FORMAT(U.created, '%Y-%m-%d %H:%i:%s') AS created,
            DATE_FORMAT(U.updated, '%Y-%m-%d %H:%i:%s') AS updated,
            COALESCE(UE.primary_email, UE.fallback_email, '') AS email,
            U.nickname,
            U.profileImageUrl,
            COALESCE(UV.visit_count, 0) AS visit_count,
            UV.last_viewed,
            UV.last_viewed_raw
          FROM User U
          LEFT JOIN (
            SELECT
              user,
              COUNT(*) AS visit_count,
              MAX(dateInserted) AS last_viewed_raw,
              DATE_FORMAT(MAX(dateInserted), '%Y-%m-%d %H:%i:%s') AS last_viewed
            FROM UserViewHistory
            GROUP BY user
          ) UV ON UV.user = U.seq
          LEFT JOIN (
            SELECT
              user,
              MIN(CASE WHEN isPrimary THEN email ELSE NULL END) AS primary_email,
              MIN(email) AS fallback_email,
              GROUP_CONCAT(email ORDER BY isPrimary DESC, email SEPARATOR ', ') AS emails
            FROM UserEmail
            GROUP BY user
          ) UE ON UE.user = U.seq
          WHERE (
            {searchIsEmpty} = TRUE OR
            UE.emails LIKE {searchLike} OR
            U.nickname LIKE {searchLike} OR
            CAST(U.seq AS CHAR) LIKE {searchLike}
          )
          ORDER BY $orderBySql
          LIMIT {pageSize} OFFSET {offset}
        """).on(
          "searchIsEmpty" -> search.isEmpty,
          "searchLike" -> searchLike,
          "pageSize" -> pageSize,
          "offset" -> offset,
        ).as((long("seq") ~ str("created") ~ str("updated") ~ str("email") ~ str("nickname") ~ str("profileImageUrl").? ~ long("visit_count") ~ str("last_viewed").?).map {
          case seq ~ created ~ updated ~ email ~ nickname ~ profileImageUrl ~ visitCount ~ lastViewed =>
            AdminUser(
              seq = seq,
              created = created,
              updated = updated,
              email = email,
              nickname = nickname,
              profileImageUrl = profileImageUrl,
              visitCount = visitCount,
              lastViewed = lastViewed,
            )
        }.*)

        Ok(pagedJson(users.asJson, page, pageSize, count))
      }
    }
  }

  def adminUserViews(userSeq: Long, n: Int = 200): Action[AnyContent] = Action { implicit request =>
    if (!isAdmin) {
      AccessDenied
    } else {
      database.withConnection { implicit connection =>
        case class AdminUserViewHistory(
          seq: Long,
          user: Long,
          site: Long,
          siteName: String,
          siteDomain: Option[String],
          pageName: String,
          viewedAt: String,
        )

        val limit = math.max(1, math.min(1000, n))

        val histories = SQL"""
          SELECT
            UV.seq,
            UV.user,
            UV.site,
            S.name AS site_name,
            SD.site_domain,
            UV.pageName,
            DATE_FORMAT(UV.dateInserted, '%Y-%m-%d %H:%i:%s') AS viewed_at
          FROM UserViewHistory UV
          INNER JOIN Site S ON S.seq = UV.site
          LEFT JOIN (
            SELECT site, MIN(domain) AS site_domain
            FROM SiteDomain
            GROUP BY site
          ) SD ON SD.site = UV.site
          WHERE UV.user = $userSeq
          ORDER BY UV.seq DESC
          LIMIT $limit
        """.as((long("seq") ~ long("user") ~ long("site") ~ str("site_name") ~ str("site_domain").? ~ str("pageName") ~ str("viewed_at")).map {
          case seq ~ user ~ site ~ siteName ~ siteDomain ~ pageName ~ viewedAt =>
            AdminUserViewHistory(
              seq = seq,
              user = user,
              site = site,
              siteName = siteName,
              siteDomain = siteDomain,
              pageName = pageName,
              viewedAt = viewedAt,
            )
        }.*)

        Ok(histories.asJson)
      }
    }
  }


  def adminDailyStats: Action[AnyContent] = Action { implicit request =>
    if (!isAdmin) {
      AccessDenied
    } else {
      database.withConnection { implicit connection =>
        case class DailyCount(ymd: String, count: Long)
        case class AdminDailyStats(
          userCreated: Seq[DailyCount],
          pageCreated: Seq[DailyCount],
          pageEdited: Seq[DailyCount],
        )

        val userCreated = SQL"""
          SELECT DATE_FORMAT(U.created, '%Y-%m-%d') ymd, COUNT(*) cnt
          FROM User U
          GROUP BY DATE_FORMAT(U.created, '%Y-%m-%d')
          ORDER BY DATE_FORMAT(U.created, '%Y-%m-%d') DESC
          LIMIT 30
        """.as((str("ymd") ~ long("cnt")).map {
          case ymd ~ cnt => DailyCount(ymd, cnt)
        }.*)

        val pageCreated = SQL"""
          SELECT DATE_FORMAT(P.dateTime, '%Y-%m-%d') ymd, COUNT(*) cnt
          FROM Page P
          WHERE P.revision = 1
          GROUP BY DATE_FORMAT(P.dateTime, '%Y-%m-%d')
          ORDER BY DATE_FORMAT(P.dateTime, '%Y-%m-%d') DESC
          LIMIT 30
        """.as((str("ymd") ~ long("cnt")).map {
          case ymd ~ cnt => DailyCount(ymd, cnt)
        }.*)

        val pageEdited = SQL"""
          SELECT DATE_FORMAT(P.dateTime, '%Y-%m-%d') ymd, COUNT(*) cnt
          FROM Page P
          GROUP BY DATE_FORMAT(P.dateTime, '%Y-%m-%d')
          ORDER BY DATE_FORMAT(P.dateTime, '%Y-%m-%d') DESC
          LIMIT 30
        """.as((str("ymd") ~ long("cnt")).map {
          case ymd ~ cnt => DailyCount(ymd, cnt)
        }.*)

        Ok(
          AdminDailyStats(
            userCreated = userCreated,
            pageCreated = pageCreated,
            pageEdited = pageEdited,
          ).asJson
        )
      }
    }
  }

  def adminTopViewedPages: Action[AnyContent] = Action { implicit request =>
    if (!isAdmin) {
      AccessDenied
    } else {
      database.withConnection { implicit connection =>
        case class AdminTopViewedPage(
          siteSeq: Long,
          siteName: String,
          siteDomain: Option[String],
          pageName: String,
          viewCount: Long,
          lastViewedAt: String,
        )

        val limit = request.getQueryString("n")
          .flatMap(raw => scala.util.Try(raw.toInt).toOption)
          .map(_.max(1).min(200))
          .getOrElse(30)

        val rows = SQL"""
          SELECT
            UV.site AS site_seq,
            S.name AS site_name,
            SD.site_domain,
            UV.pageName AS page_name,
            COUNT(*) AS view_count,
            DATE_FORMAT(MAX(UV.dateInserted), '%Y-%m-%d %H:%i:%s') AS last_viewed_at
          FROM UserViewHistory UV
          INNER JOIN Site S ON S.seq = UV.site
          LEFT JOIN (
            SELECT site, MIN(domain) AS site_domain
            FROM SiteDomain
            GROUP BY site
          ) SD ON SD.site = UV.site
          GROUP BY UV.site, S.name, SD.site_domain, UV.pageName
          ORDER BY view_count DESC, MAX(UV.dateInserted) DESC
          LIMIT $limit
        """.as((long("site_seq") ~ str("site_name") ~ str("site_domain").? ~ str("page_name") ~ long("view_count") ~ str("last_viewed_at")).map {
          case siteSeq ~ siteName ~ siteDomain ~ pageName ~ viewCount ~ lastViewedAt =>
            AdminTopViewedPage(
              siteSeq = siteSeq,
              siteName = siteName,
              siteDomain = siteDomain,
              pageName = pageName,
              viewCount = viewCount,
              lastViewedAt = lastViewedAt,
            )
        }.*)

        Ok(rows.asJson)
      }
    }
  }

  def adminRecentChanges: Action[AnyContent] = Action { implicit request =>
    if (!isAdmin) {
      AccessDenied
    } else {
      database.withConnection { implicit connection =>
        case class AdminRecentChange(
          siteSeq: Long,
          siteName: String,
          siteDomain: Option[String],
          name: String,
          revision: Long,
          dateTime: String,
          nickname: Option[String],
          remoteAddress: String,
          comment: String,
          isMinorEdit: Boolean,
          viaApi: Boolean,
          userApiKeyName: Option[String],
        )

        val limit = request.getQueryString("n")
          .flatMap(raw => scala.util.Try(raw.toInt).toOption)
          .map(_.max(1).min(500))
          .getOrElse(50)
        val includeMinorEdit = request.getQueryString("includeMinorEdit")
          .flatMap(raw => scala.util.Try(raw.toInt).toOption)
          .contains(1)
        val includeViaApi = request.getQueryString("includeViaApi")
          .flatMap(raw => scala.util.Try(raw.toInt).toOption)
          .contains(1)

        val rows = SQL"""
          SELECT
            P.site AS site_seq,
            S.name AS site_name,
            SD.site_domain,
            P.name,
            P.revision,
            DATE_FORMAT(P.dateTime, '%Y-%m-%d %H:%i:%s') AS date_time,
            U.nickname,
            P.remoteAddress,
            P.comment,
            P.isMinorEdit,
            P.viaApi,
            AK.name AS userApiKeyName
          FROM Page P
          INNER JOIN Site S ON S.seq = P.site
          LEFT JOIN (
            SELECT site, MIN(domain) AS site_domain
            FROM SiteDomain
            GROUP BY site
          ) SD ON SD.site = P.site
          LEFT JOIN User U ON U.seq = P.user
          LEFT JOIN UserApiKey AK ON AK.seq = P.userApiKey
          WHERE (${includeMinorEdit} OR P.isMinorEdit = false)
            AND (${includeViaApi} OR P.viaApi = false)
          ORDER BY P.dateTime DESC
          LIMIT $limit
        """.as((long("site_seq") ~ str("site_name") ~ str("site_domain").? ~ str("name") ~ long("revision") ~ str("date_time") ~ str("nickname").? ~ str("remoteAddress") ~ str("comment") ~ bool("isMinorEdit") ~ bool("viaApi") ~ str("userApiKeyName").?).map {
          case siteSeq ~ siteName ~ siteDomain ~ name ~ revision ~ dateTime ~ nickname ~ remoteAddress ~ comment ~ isMinorEdit ~ viaApi ~ userApiKeyName =>
            AdminRecentChange(
              siteSeq = siteSeq,
              siteName = siteName,
              siteDomain = siteDomain,
              name = name,
              revision = revision,
              dateTime = dateTime,
              nickname = nickname,
              remoteAddress = remoteAddress,
              comment = comment,
              isMinorEdit = isMinorEdit,
              viaApi = viaApi,
              userApiKeyName = userApiKeyName,
            )
        }.*)

        Ok(rows.asJson)
      }
    }
  }

  def adminAccessLogs: Action[AnyContent] = Action { implicit request =>
    val requestedSiteSeq = request.getQueryString("siteSeq")
      .flatMap(raw => scala.util.Try(raw.toLong).toOption)
      .filter(_ > 0)
    val permitted = isAdmin || requestedSiteSeq.exists(seq => isSiteAdmin(seq))
    if (!permitted) {
      AccessDenied
    } else {
      database.withConnection { implicit connection =>
        case class AdminAccessLog(
          seq: Long,
          siteSeq: Long,
          siteName: String,
          ipDenySeq: Option[Long],
          userSeq: Option[Long],
          method: String,
          scheme: String,
          host: String,
          uri: String,
          status: Int,
          remoteAddress: String,
          durationMilli: Int,
          userAgent: String,
          dateInserted: String,
        )

        val page = request.getQueryString("page")
          .flatMap(raw => scala.util.Try(raw.toInt).toOption)
          .map(_.max(1))
          .getOrElse(1)
        val pageSize = request.getQueryString("pageSize")
          .flatMap(raw => scala.util.Try(raw.toInt).toOption)
          .map(_.max(1).min(1000))
          .getOrElse(20)
        val search = request.getQueryString("search").map(_.trim).getOrElse("")
        // SiteAdmin은 자신의 사이트로 siteSeq 강제 적용
        val siteSeq = if (isAdmin) {
          request.getQueryString("siteSeq")
            .flatMap(raw => scala.util.Try(raw.toLong).toOption)
            .filter(_ > 0)
        } else {
          requestedSiteSeq
        }
        val sortByRaw = request.getQueryString("sortBy").map(_.trim).getOrElse("seq")
        val sortOrderRaw = request.getQueryString("sortOrder").map(_.trim.toLowerCase).getOrElse("desc")

        val sortBy = sortByRaw match {
          case "dateInserted" => "AL.dateInserted"
          case "status" => "AL.status"
          case "durationMilli" => "AL.durationMilli"
          case "remoteAddress" => "AL.remoteAddress"
          case "method" => "AL.method"
          case "uri" => "AL.uri"
          case _ => "AL.seq"
        }
        val sortOrder = if (sortOrderRaw == "asc") "ASC" else "DESC"
        val offset = (page - 1) * pageSize
        val searchLike = s"%$search%"
        val siteSeqIsEmpty = siteSeq.isEmpty
        val siteSeqValue: Long = siteSeq.getOrElse(0L)
        val searchIsEmpty = search.isEmpty

        val count = SQL"""
          SELECT COUNT(*) AS count_value
          FROM AccessLog AL
          INNER JOIN Site S ON S.seq = AL.site
          WHERE (
            $siteSeqIsEmpty = TRUE OR AL.site = $siteSeqValue
          ) AND (
            $searchIsEmpty = TRUE OR
            S.name LIKE $searchLike OR
            AL.method LIKE $searchLike OR
            AL.uri LIKE $searchLike OR
            AL.remoteAddress LIKE $searchLike OR
            AL.userAgent LIKE $searchLike
          )
        """.as(long("count_value").single)

        val orderBySql = s"$sortBy $sortOrder"
        val rows = SQL(s"""
          SELECT
            AL.seq,
            AL.site AS site_seq,
            S.name AS site_name,
            AL.ipDeny AS ip_deny_seq,
            AL.user AS user_seq,
            AL.method,
            AL.scheme,
            AL.host,
            AL.uri,
            AL.status,
            AL.remoteAddress,
            AL.durationMilli,
            AL.userAgent,
            DATE_FORMAT(AL.dateInserted, '%Y-%m-%d %H:%i:%s') AS date_inserted
          FROM AccessLog AL
          INNER JOIN Site S ON S.seq = AL.site
          WHERE (
            {siteSeqIsEmpty} = TRUE OR AL.site = {siteSeq}
          ) AND (
            {searchIsEmpty} = TRUE OR
            S.name LIKE {searchLike} OR
            AL.method LIKE {searchLike} OR
            AL.uri LIKE {searchLike} OR
            AL.remoteAddress LIKE {searchLike} OR
            AL.userAgent LIKE {searchLike}
          )
          ORDER BY $orderBySql
          LIMIT {pageSize} OFFSET {offset}
        """).on(
          "searchIsEmpty" -> searchIsEmpty,
          "siteSeqIsEmpty" -> siteSeqIsEmpty,
          "siteSeq" -> siteSeqValue,
          "searchLike" -> searchLike,
          "pageSize" -> pageSize,
          "offset" -> offset,
        ).as((long("seq") ~ long("site_seq") ~ str("site_name") ~ long("ip_deny_seq").? ~ long("user_seq").? ~ str("method") ~ str("scheme") ~ str("host") ~ str("uri") ~ int("status") ~ str("remoteAddress") ~ int("durationMilli") ~ str("userAgent") ~ str("date_inserted")).map {
          case seq ~ siteSeq ~ siteName ~ ipDenySeq ~ userSeq ~ method ~ scheme ~ host ~ uri ~ status ~ remoteAddress ~ durationMilli ~ userAgent ~ dateInserted =>
            AdminAccessLog(
              seq = seq,
              siteSeq = siteSeq,
              siteName = siteName,
              ipDenySeq = ipDenySeq,
              userSeq = userSeq,
              method = method,
              scheme = scheme,
              host = host,
              uri = uri,
              status = status,
              remoteAddress = remoteAddress,
              durationMilli = durationMilli,
              userAgent = userAgent,
              dateInserted = dateInserted,
            )
        }.*)

        Ok(pagedJson(rows.asJson, page, pageSize, count))
      }
    }
  }
}

object ApiAdminReport {
  /** How many months the monthly series goes back, the current one included. */
  val monthsReported = 12

  /** The window of the per-site counts, in days back from now. */
  val activeWindowDays = 30

  case class MonthlySiteActivity(
    ym: String,
    sitesCreated: Long,
    sitesActive: Long,
    pagesCreated: Long,
    pagesEdited: Long,
  )

  case class SiteActivity(
    siteSeq: Long,
    siteName: String,
    siteDomain: Option[String],
    created: String,
    pagesCreated30d: Long,
    pagesEdited30d: Long,
    editors30d: Long,
    firstEditedAt: Option[String],
    lastEditedAt: Option[String],
  )

  case class SiteActivityReport(months: Seq[MonthlySiteActivity], sites: Seq[SiteActivity])

  private val dateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")

  /**
   * A site is active in a month when any revision of any of its pages was saved in that month.
   * Minor edits and edits through the API count: the question is whether anyone is writing
   * there at all, and an owner whose agent writes for them is still using the site. Views do
   * not count — a site nobody writes to is not in use, however often it is read.
   *
   * The SQL is YEAR() and MONTH() rather than DATE_FORMAT(), which H2 does not have, so that
   * the spec can run this against the test schema. The other reports in this file predate
   * that concern and cannot be run there.
   *
   * `now` is a parameter so the spec can fix it. Every month in the series is returned, with
   * zeros where nothing happened — a month missing from a table reads as a gap in the data,
   * not as a month of no activity.
   *
   * Each site also carries its first and last edit ever. The first, read against `created`,
   * says whether a site was written to soon after it was opened; the last says whether it has
   * gone quiet. Neither is limited to the window.
   */
  def siteActivity(now: LocalDateTime)(implicit connection: Connection): SiteActivityReport = {
    val currentMonth = YearMonth.from(now)
    val months = (monthsReported - 1 to 0 by -1).map(currentMonth.minusMonths(_))
    val firstMonthStart = months.head.atDay(1).atStartOfDay()

    def yearMonth(year: Int, month: Int): YearMonth = YearMonth.of(year, month)

    val sitesCreatedByMonth: Map[YearMonth, Long] = SQL"""
      SELECT YEAR(S.created) AS y, MONTH(S.created) AS m, COUNT(*) AS cnt
      FROM Site S
      WHERE S.created >= $firstMonthStart
      GROUP BY YEAR(S.created), MONTH(S.created)
    """.as((int("y") ~ int("m") ~ long("cnt")).map { case y ~ m ~ cnt => yearMonth(y, m) -> cnt }.*).toMap

    val pagesByMonth: Map[YearMonth, (Long, Long, Long)] = SQL"""
      SELECT
        YEAR(P.dateTime) AS y,
        MONTH(P.dateTime) AS m,
        COUNT(DISTINCT P.site) AS sites_active,
        SUM(CASE WHEN P.revision = 1 THEN 1 ELSE 0 END) AS pages_created,
        COUNT(*) AS pages_edited
      FROM Page P
      WHERE P.dateTime >= $firstMonthStart
      GROUP BY YEAR(P.dateTime), MONTH(P.dateTime)
    """.as((int("y") ~ int("m") ~ long("sites_active") ~ long("pages_created") ~ long("pages_edited")).map {
      case y ~ m ~ sitesActive ~ pagesCreated ~ pagesEdited => yearMonth(y, m) -> (sitesActive, pagesCreated, pagesEdited)
    }.*).toMap

    val monthly = months.map { month =>
      val (sitesActive, pagesCreated, pagesEdited) = pagesByMonth.getOrElse(month, (0L, 0L, 0L))
      MonthlySiteActivity(
        ym = month.toString,
        sitesCreated = sitesCreatedByMonth.getOrElse(month, 0L),
        sitesActive = sitesActive,
        pagesCreated = pagesCreated,
        pagesEdited = pagesEdited,
      )
    }

    val windowStart = now.minusDays(activeWindowDays)
    val sites = SQL"""
      SELECT
        S.seq AS site_seq,
        S.name AS site_name,
        SD.site_domain,
        S.created,
        COALESCE(W.pages_created, 0) AS pages_created,
        COALESCE(W.pages_edited, 0) AS pages_edited,
        COALESCE(W.editors, 0) AS editors,
        L.first_edited_at,
        L.last_edited_at
      FROM Site S
      LEFT JOIN (
        SELECT site, MIN(domain) AS site_domain
        FROM SiteDomain
        GROUP BY site
      ) SD ON SD.site = S.seq
      LEFT JOIN (
        SELECT
          P.site,
          SUM(CASE WHEN P.revision = 1 THEN 1 ELSE 0 END) AS pages_created,
          COUNT(*) AS pages_edited,
          COUNT(DISTINCT P.user) AS editors
        FROM Page P
        WHERE P.dateTime >= $windowStart
        GROUP BY P.site
      ) W ON W.site = S.seq
      LEFT JOIN (
        SELECT P.site, MIN(P.dateTime) AS first_edited_at, MAX(P.dateTime) AS last_edited_at
        FROM Page P
        GROUP BY P.site
      ) L ON L.site = S.seq
      ORDER BY pages_edited DESC, L.last_edited_at DESC, S.seq
    """.as((long("site_seq") ~ str("site_name") ~ str("site_domain").? ~ localDateTime("created") ~ long("pages_created") ~ long("pages_edited") ~ long("editors") ~ localDateTime("first_edited_at").? ~ localDateTime("last_edited_at").?).map {
      case siteSeq ~ siteName ~ siteDomain ~ created ~ pagesCreated ~ pagesEdited ~ editors ~ firstEditedAt ~ lastEditedAt =>
        SiteActivity(
          siteSeq = siteSeq,
          siteName = siteName,
          siteDomain = siteDomain,
          created = created.format(dateTimeFormatter),
          pagesCreated30d = pagesCreated,
          pagesEdited30d = pagesEdited,
          editors30d = editors,
          firstEditedAt = firstEditedAt.map(_.format(dateTimeFormatter)),
          lastEditedAt = lastEditedAt.map(_.format(dateTimeFormatter)),
        )
    }.*)

    SiteActivityReport(months = monthly, sites = sites)
  }
}
