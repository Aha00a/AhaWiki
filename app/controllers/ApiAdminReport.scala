package controllers

import anorm.SqlParser.{bool, get, int, long, str}
import anorm._
import com.aha00a.commons.Implicits._
import io.circe.generic.auto._
import io.circe.syntax._
import models.tables.Site
import play.api.Logging
import play.api.db.Database
import play.api.mvc._

import javax.inject._

/**
 * What the admin screens report on: users, page views, daily counts, recent changes, and
 * the access log.
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

        Ok(Map(
          "array" -> users.asJson,
          "page" -> page.asJson,
          "pageSize" -> pageSize.asJson,
          "count" -> count.asJson,
        ).asJson)
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

        Ok(Map(
          "array" -> rows.asJson,
          "page" -> page.asJson,
          "pageSize" -> pageSize.asJson,
          "count" -> count.asJson,
        ).asJson)
      }
    }
  }
}
