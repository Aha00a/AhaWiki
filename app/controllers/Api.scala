package controllers

import akka.actor.ActorRef
import akka.actor.ActorSystem
import anorm.SqlParser.{long, str}
import anorm._
import com.aha00a.play.Implicits.RichRequest
import io.circe.Json
import io.circe.generic.auto._
import io.circe.syntax._
import logics.AhaWikiCache
import logics.ApplicationConf
import logics.SessionLogic
import logics.SiteLogic
import logics.wikis.PageLogic
import logics.wikis.SignedReadUrlLogic
import logics.wikis.ExtractConvertInjectMacro
import logics.wikis.interpreters.Interpreters
import models.Adjacent
import models.ContextSite
import models.ContextWikiPage
import models.RequestWrapper
import models.tables.CalculatedLink
import models.tables.Page
import models.tables.PageWithoutContentWithSize
import models.tables.Site
import models.tables.UserSite
import play.api.Configuration
import play.api.db.Database
import play.api.libs.ws.WSClient
import play.api.mvc._
import play.filters.csrf.CSRF
import services.ApplicationLifecycleHook

import java.net.URLDecoder
import javax.inject._
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.DurationInt
import scala.util.Random


class Api @Inject()(
  implicit val
  controllerComponents: ControllerComponents,
  actorSystem: ActorSystem,
  database: Database,
  @Named("db-actor") actorAhaWiki: ActorRef,
  applicationConf: ApplicationConf,
  ahaWikiCache: AhaWikiCache,
  wsClient: WSClient,
  executionContext: ExecutionContext,
  applicationLifecycleHook: ApplicationLifecycleHook,
  configuration: Configuration
) extends BaseController {
  private def isAdmin(implicit request: RequestHeader): Boolean = {
    SessionLogic.getUser(request).exists(u => u.email == "aha00a@gmail.com" || u.seq == 1)
  }

  def Ok(json: io.circe.Json): Result = Ok(json.toString()).as(JSON)


  private lazy val signedReadUrlSecret: String = configuration.getOptional[String]("play.http.secret.key").getOrElse("")


  def adminGenerateSignedReadUrl: Action[AnyContent] = Action { implicit request =>
    if (!isAdmin) {
      Forbidden("Access denied.")
    } else {
      val name = request.getQueryString("name").map(_.trim).getOrElse("")
      val revision = request.getQueryString("revision").flatMap(v => scala.util.Try(v.toInt).toOption).getOrElse(0)
      val actionInput = request.getQueryString("action").getOrElse("")
      val action = actionInput match {
        case "" | "view" => "view"
        case "raw" => "raw"
        case "history" => "history"
        case "diff" => "diff"
        case _ => ""
      }

      if (signedReadUrlSecret.isEmpty) {
        InternalServerError(Json.obj("error" -> Json.fromString("Signed URL secret is not configured.")).toString()).as(JSON)
      } else if (name.isEmpty) {
        BadRequest(Json.obj("error" -> Json.fromString("Query parameter 'name' is required.")).toString()).as(JSON)
      } else if (action.isEmpty) {
        BadRequest(Json.obj("error" -> Json.fromString("action must be one of: view, raw, history, diff")).toString()).as(JSON)
      } else {
        val expiresAt = java.time.Instant.now().getEpochSecond + SignedReadUrlLogic.ValidDurationSeconds
        val signature = SignedReadUrlLogic.signReadRequest(
          host = request.host,
          name = name,
          revision = revision,
          action = action,
          expiresAtEpochSeconds = expiresAt,
          secret = signedReadUrlSecret,
        )

        val basePath = routes.Wiki.view(java.net.URLEncoder.encode(name, "UTF-8").replace("+", "%20"), revision, action).url
        val separator = if (basePath.contains("?")) "&" else "?"
        val signedPath = s"$basePath$separator${SignedReadUrlLogic.QueryParamExpires}=$expiresAt&${SignedReadUrlLogic.QueryParamSignature}=$signature"

        Ok(Json.obj(
          "name" -> Json.fromString(name),
          "revision" -> Json.fromInt(revision),
          "action" -> Json.fromString(action),
          "expiresAtEpochSeconds" -> Json.fromLong(expiresAt),
          "signedPath" -> Json.fromString(signedPath),
          "signedUrl" -> Json.fromString(s"${request.scheme}://${request.host}$signedPath"),
        ).toString()).as(JSON)
      }
    }
  }

  def adminSites: Action[AnyContent] = Action { implicit request =>
    if (!isAdmin) {
      Forbidden("Access denied.")
    } else {
      database.withConnection { implicit connection =>
        case class AdminSite(seq: Long, name: String, domains: Seq[String], userCount: Long, pageCount: Long)
        case class AdminSiteRow(seq: Long, name: String, domain: Option[String], userCount: Long, pageCount: Long)

        val rows = SQL"""
          SELECT
            S.seq,
            S.name,
            SD.domain,
            COALESCE(US.user_count, 0) AS user_count,
            COALESCE(P.page_count, 0) AS page_count
          FROM Site S
          LEFT JOIN SiteDomain SD ON SD.site = S.seq
          LEFT JOIN (
            SELECT site, COUNT(*) AS user_count
            FROM UserSite
            GROUP BY site
          ) US ON US.site = S.seq
          LEFT JOIN (
            SELECT site, COUNT(*) AS page_count
            FROM Page
            GROUP BY site
          ) P ON P.site = S.seq
          ORDER BY S.seq, SD.domain
        """.as((long("seq") ~ str("name") ~ str("domain").? ~ long("user_count") ~ long("page_count")).map {
          case seq ~ name ~ domain ~ userCount ~ pageCount => AdminSiteRow(seq, name, domain, userCount, pageCount)
        }.*)

        val sites = rows
          .groupBy(r => (r.seq, r.name, r.userCount, r.pageCount))
          .toSeq
          .sortBy(_._1._1)
          .map { case ((seq, name, userCount, pageCount), groupedRows) =>
            AdminSite(
              seq = seq,
              name = name,
              domains = groupedRows.flatMap(_.domain).distinct.sorted,
              userCount = userCount,
              pageCount = pageCount,
            )
          }

        Ok(sites.asJson)
      }
    }
  }

  def adminSiteUsers: Action[AnyContent] = Action { implicit request =>
    if (!isAdmin) {
      Forbidden("Access denied.")
    } else {
      database.withConnection { implicit connection =>
        implicit val site: Site = SiteLogic.get(request.host)
        case class AdminSiteUser(user: Long, site: Long, created: String, email: String, nickname: String)
        val users = UserSite.select().map(u => AdminSiteUser(u.user, u.site, u.created.toInstant.toString, u.email, u.nickname))
        Ok(users.asJson)
      }
    }
  }

  def adminUsers: Action[AnyContent] = Action { implicit request =>
    if (!isAdmin) {
      Forbidden("Access denied.")
    } else {
      database.withConnection { implicit connection =>
        case class AdminUser(
          seq: Long,
          created: String,
          updated: String,
          email: String,
          nickname: String,
          siteCount: Long,
          visitCount: Long,
          lastViewed: Option[String],
        )

        val users = SQL"""
          SELECT
            U.seq,
            DATE_FORMAT(U.created, '%Y-%m-%d %H:%i:%s') AS created,
            DATE_FORMAT(U.updated, '%Y-%m-%d %H:%i:%s') AS updated,
            U.email,
            U.nickname,
            COALESCE(US.site_count, 0) AS site_count,
            COALESCE(UV.visit_count, 0) AS visit_count,
            UV.last_viewed
          FROM User U
          LEFT JOIN (
            SELECT user, COUNT(*) AS site_count
            FROM UserSite
            GROUP BY user
          ) US ON US.user = U.seq
          LEFT JOIN (
            SELECT
              user,
              COUNT(*) AS visit_count,
              DATE_FORMAT(MAX(dateInserted), '%Y-%m-%d %H:%i:%s') AS last_viewed
            FROM UserViewHistory
            GROUP BY user
          ) UV ON UV.user = U.seq
          ORDER BY
            CASE WHEN UV.last_viewed IS NULL THEN 1 ELSE 0 END ASC,
            UV.last_viewed DESC,
            U.seq DESC
        """.as((long("seq") ~ str("created") ~ str("updated") ~ str("email") ~ str("nickname") ~ long("site_count") ~ long("visit_count") ~ str("last_viewed").?).map {
          case seq ~ created ~ updated ~ email ~ nickname ~ siteCount ~ visitCount ~ lastViewed =>
            AdminUser(
              seq = seq,
              created = created,
              updated = updated,
              email = email,
              nickname = nickname,
              siteCount = siteCount,
              visitCount = visitCount,
              lastViewed = lastViewed,
            )
        }.*)

        Ok(users.asJson)
      }
    }
  }

  def adminUserViews(userSeq: Long, n: Int = 200): Action[AnyContent] = Action { implicit request =>
    if (!isAdmin) {
      Forbidden("Access denied.")
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


  def adminSchedulers: Action[AnyContent] = Action { implicit request =>
    if (!isAdmin) {
      Forbidden("Access denied.")
    } else {
      case class AdminScheduler(
        name: String,
        minSeconds: Int,
        maxSeconds: Int,
        running: Boolean,
        nextDelaySeconds: Option[Int],
        lastStartedAt: Option[String],
        lastFinishedAt: Option[String],
        lastResult: Option[String],
        runCount: Long,
      )

      val schedulers = applicationLifecycleHook.getSchedulerStatuses.map { scheduler =>
        AdminScheduler(
          scheduler.name,
          scheduler.minSeconds,
          scheduler.maxSeconds,
          scheduler.running,
          scheduler.nextDelaySeconds,
          scheduler.lastStartedAt,
          scheduler.lastFinishedAt,
          scheduler.lastResult,
          scheduler.runCount,
        )
      }

      Ok(schedulers.asJson)
    }
  }

  def adminDailyStats: Action[AnyContent] = Action { implicit request =>
    if (!isAdmin) {
      Forbidden("Access denied.")
    } else {
      database.withConnection { implicit connection =>
        case class DailyCount(ymd: String, count: Long)
        case class AdminDailyStats(
          userCreated: Seq[DailyCount],
          siteUserCreated: Seq[DailyCount],
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

        val siteUserCreated = SQL"""
          SELECT DATE_FORMAT(US.created, '%Y-%m-%d') ymd, COUNT(*) cnt
          FROM UserSite US
          GROUP BY DATE_FORMAT(US.created, '%Y-%m-%d')
          ORDER BY DATE_FORMAT(US.created, '%Y-%m-%d') DESC
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
            siteUserCreated = siteUserCreated,
            pageCreated = pageCreated,
            pageEdited = pageEdited,
          ).asJson
        )
      }
    }
  }

  def adminRecentChanges: Action[AnyContent] = Action { implicit request =>
    if (!isAdmin) {
      Forbidden("Access denied.")
    } else {
      database.withConnection { implicit connection =>
        case class AdminRecentChange(
          siteSeq: Long,
          siteName: String,
          name: String,
          revision: Long,
          dateTime: String,
          nickname: Option[String],
          remoteAddress: String,
          comment: String,
        )

        val limit = request.getQueryString("n")
          .flatMap(raw => scala.util.Try(raw.toInt).toOption)
          .map(_.max(1).min(500))
          .getOrElse(50)

        val rows = SQL"""
          SELECT
            P.site AS site_seq,
            S.name AS site_name,
            P.name,
            P.revision,
            DATE_FORMAT(P.dateTime, '%Y-%m-%d %H:%i:%s') AS date_time,
            U.nickname,
            P.remoteAddress,
            P.comment
          FROM Page P
          INNER JOIN Site S ON S.seq = P.site
          LEFT JOIN User U ON U.seq = P.user
          ORDER BY P.dateTime DESC
          LIMIT $limit
        """.as((long("site_seq") ~ str("site_name") ~ str("name") ~ long("revision") ~ str("date_time") ~ str("nickname").? ~ str("remoteAddress") ~ str("comment")).map {
          case siteSeq ~ siteName ~ name ~ revision ~ dateTime ~ nickname ~ remoteAddress ~ comment =>
            AdminRecentChange(
              siteSeq = siteSeq,
              siteName = siteName,
              name = name,
              revision = revision,
              dateTime = dateTime,
              nickname = nickname,
              remoteAddress = remoteAddress,
              comment = comment,
            )
        }.*)

        Ok(rows.asJson)
      }
    }
  }

  def adminRunScheduler(name: String): Action[AnyContent] = Action { implicit request =>
    if (!isAdmin) {
      Forbidden("Access denied.")
    } else {
      if (applicationLifecycleHook.runSchedulerNow(name)) {
        Ok(Map("status" -> "queued", "name" -> name).asJson)
      } else {
        NotFound(name)
      }
    }
  }

  def csrf: Action[AnyContent] = Action { implicit request =>
    val token: Option[CSRF.Token] = CSRF.getToken
    Ok(token.asJson)
  }

  def pageMap: Action[AnyContent] = Action { implicit request =>
    database.withConnection { implicit connection =>
      implicit val site: Site = SiteLogic.get(request.host)
      val listLink = Random.shuffle(CalculatedLink.selectAllButNotEmpty()).take(10)
      Ok(listLink.asJson)
    }
  }

  def pageNames: Action[AnyContent] = Action { implicit request =>
    database.withConnection { implicit connection =>
      implicit val site: Site = SiteLogic.get(request.host)
      implicit val contextSite: ContextSite = ContextSite()
      implicit val requestWrapper: RequestWrapper = RequestWrapper()
      Ok(PageLogic.getListPageByPermission().map(_.name).asJson)
    }
  }

  def macroNames: Action[AnyContent] = Action { implicit request =>
    Ok(ExtractConvertInjectMacro.macroNames.asJson)
  }

  def interpreterNames: Action[AnyContent] = Action { implicit request =>
    Ok(Interpreters.map.values.map(_.name).toSeq.distinct.sorted.asJson)
  }


  def links(nameEncoded: String): Action[AnyContent] = Action { implicit request =>
    val name = URLDecoder.decode(nameEncoded.replace("+", "%2B"), "UTF-8")
    database.withConnection { implicit connection =>
      implicit val site: Site = SiteLogic.get(request.host)
      implicit val contextSite: ContextSite = ContextSite()
      Ok(Adjacent.getSeqLinkFiltered(name).asJson)
    }
  }

  def statistics(): Action[AnyContent] = Action { implicit request =>
    database.withConnection { implicit connection =>
      implicit val site: Site = SiteLogic.get(request.host)
      implicit val contextWikiPage: ContextWikiPage = ContextWikiPage("")

      val seqPage: Seq[PageWithoutContentWithSize] = contextWikiPage.seqPageByPermission
      val selectYmdCountOfFirstRevision: Seq[(String, Long)] = Page.selectYmdCountOfFirstRevision()

      val totalSize: Long = seqPage.map(_.size).sum

      val value1: Map[String, Seq[(String, Long)]] = Map(
        "arrayArrayYmdCountOfFirstRevision" -> selectYmdCountOfFirstRevision,
      )
      val value2: Map[String, Long] = Map(
        "totalSize" -> totalSize,
        "pageCount" -> seqPage.length,
        "count" -> seqPage.length,
      )

      val json1: Json = value1.asJson
      val json2: Json = value2.asJson

      Ok(json1.deepMerge(json2))
    }
  }

  def cacheDelete(siteSeq: Long): Action[AnyContent] = Action { implicit request =>
    SiteLogic.get(siteSeq) foreach { implicit site =>
      implicit val tupleDatabaseSite: (Database, Site) = (database, site)
      implicit val contextSite: ContextSite = ContextSite()
      Seq(
        () => { ahaWikiCache.SiteDomain.invalidate() },
        () => { ahaWikiCache.SiteDomain.Map.invalidate() },
        () => { ahaWikiCache.Site.invalidate() },
        () => { ahaWikiCache.Site.Map.invalidate() },
        () => { ahaWikiCache.Page.SeqPageWithoutContentWithSizeLatest.invalidate() },
        () => { ahaWikiCache.Header.invalidate() },
      ).zipWithIndex foreach { case (f, i) =>
        actorSystem.scheduler.scheduleOnce((2 * i) second) {f()}
      }
    }
    Ok("ok")
  }
}
