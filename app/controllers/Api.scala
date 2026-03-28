package controllers

import akka.actor.ActorRef
import akka.actor.ActorSystem
import anorm.SqlParser.{long, str}
import anorm._
import io.circe.Json
import io.circe.generic.auto._
import io.circe.syntax._
import logics.AhaWikiCache
import logics.ApplicationConf
import logics.SessionLogic
import logics.SiteLogic
import logics.wikis.PageLogic
import models.Adjacent
import models.ContextSite
import models.ContextWikiPage
import models.RequestWrapper
import models.tables.CalculatedLink
import models.tables.Page
import models.tables.PageWithoutContentWithSize
import models.tables.Site
import models.tables.UserSite
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
  applicationLifecycleHook: ApplicationLifecycleHook
) extends BaseController {
  private def isAdmin(implicit request: RequestHeader): Boolean = {
    SessionLogic.getUser(request).exists(u => u.email == "aha00a@gmail.com" || u.seq == 1)
  }

  def Ok(json: io.circe.Json): Result = Ok(json.toString()).as(JSON)

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
