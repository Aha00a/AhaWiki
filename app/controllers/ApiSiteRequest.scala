package controllers

import io.circe.Json
import logics.AhaWikiCache
import logics.AhaWikiCacheMemoryDomainSite
import logics.ApplicationConf
import logics.SessionLogic
import logics.SiteLogic
import logics.SiteRequestLogic
import logics.TelegramLogic
import models.ContextSite
import models.WikiActors
import models.tables.Site
import models.tables.SiteRequest
import models.tables.User
import play.api.db.Database
import play.api.libs.ws.WSClient
import play.api.mvc._

import javax.inject._
import scala.concurrent.ExecutionContext

/**
 * Site requests, from both ends. The page is a plain form, so that asking for a wiki needs no
 * script; the admin screen speaks JSON like its neighbours.
 */
class ApiSiteRequest @Inject()(
  implicit val
  controllerComponents: ControllerComponents,
  database: Database,
  wikiActors: WikiActors,
  applicationConf: ApplicationConf,
  ahaWikiCache: AhaWikiCache,
  telegramLogic: TelegramLogic,
  wsClient: WSClient,
  executionContext: ExecutionContext,
) extends BaseController with JsonResults with AdminAuth {

  private val rejectReasonMaxLength = 1000

  private def parentDomain: String = applicationConf.AhaWiki.siteRequest.parentDomain()

  def page: Action[AnyContent] = Action { implicit request =>
    database.withConnection { implicit connection =>
      implicit val site: Site = SiteLogic.get(request.host)
      implicit val contextSite: ContextSite = ContextSite()
      val user = SessionLogic.getUser(request)
      val rows = user.map(u => SiteRequest.selectByUser(u.seq, SiteRequestLogic.RecentRequestsPerUser)).getOrElse(Seq.empty)
      Ok(views.html.Account.siteRequest(user.isDefined, parentDomain, rows)).withHeaders("X-Robots-Tag" -> "noindex, nofollow")
    }
  }

  private def formField(name: String)(implicit request: Request[AnyContent]): String =
    request.body.asFormUrlEncoded.flatMap(_.get(name).flatMap(_.headOption)).getOrElse("")

  def submit: Action[AnyContent] = Action { implicit request =>
    SessionLogic.getUser(request) match {
      case None => Redirect(routes.Auth.login).flashing("error" -> "Login required.")
      case Some(user) =>
        val result = database.withTransaction { implicit connection =>
          SiteRequestLogic.request(user.seq, formField("name"), formField("subdomain"), formField("isPublic") == "true", formField("purpose"), parentDomain)
        }
        result match {
          case Left(error) => Redirect(routes.ApiSiteRequest.page).flashing("error" -> error.message)
          case Right(row) =>
            telegramLogic.notifySiteRequested(request.host, row.name, SiteRequestLogic.domainOf(row.subdomain, parentDomain), user.nickname, row.purpose)
            Redirect(routes.ApiSiteRequest.page).flashing("success" -> "Your request was received. You will see the answer on this page.")
        }
    }
  }

  def cancel(seq: Long): Action[AnyContent] = Action { implicit request =>
    SessionLogic.getUser(request) match {
      case None => Redirect(routes.Auth.login).flashing("error" -> "Login required.")
      case Some(user) =>
        database.withTransaction(implicit connection => SiteRequestLogic.cancel(seq, user.seq)) match {
          case Left(error) => Redirect(routes.ApiSiteRequest.page).flashing("error" -> error.message)
          case Right(_) => Redirect(routes.ApiSiteRequest.page).flashing("success" -> "Your request was withdrawn.")
        }
    }
  }

  private def requestJson(row: SiteRequest, user: Option[User]): Json = Json.obj(
    "seq" -> Json.fromLong(row.seq),
    "user" -> Json.fromLong(row.user),
    "userNickname" -> user.map(u => Json.fromString(u.nickname)).getOrElse(Json.Null),
    "name" -> Json.fromString(row.name),
    "domain" -> Json.fromString(SiteRequestLogic.domainOf(row.subdomain, parentDomain)),
    "isPublic" -> Json.fromBoolean(row.isPublic),
    "purpose" -> Json.fromString(row.purpose),
    "status" -> Json.fromString(row.status),
    "rejectReason" -> row.rejectReason.map(Json.fromString).getOrElse(Json.Null),
    "dateInserted" -> Json.fromString(row.dateInserted.toString),
    "dateReviewed" -> row.dateReviewed.map(v => Json.fromString(v.toString)).getOrElse(Json.Null),
  )

  def adminSiteRequests(status: String): Action[AnyContent] = Action { implicit request =>
    if (!isAdmin) {
      AccessDenied
    } else {
      database.withConnection { implicit connection =>
        val rows = SiteRequest.Status.values.find(_.toString.equalsIgnoreCase(status)) match {
          case Some(value) => SiteRequest.selectByStatus(value)
          case None => SiteRequest.selectAll(200)
        }
        Ok(Json.fromValues(rows.map(row => requestJson(row, User.selectBySeq(row.user)))))
      }
    }
  }

  def adminApprove(seq: Long): Action[AnyContent] = Action { implicit request =>
    withAdminReview(seq) { (adminSeq, _) => implicit connection =>
      SiteRequestLogic.approve(seq, adminSeq, parentDomain)
    }
  }

  def adminReject(seq: Long): Action[AnyContent] = Action { implicit request =>
    withAdminReview(seq) { (adminSeq, reason) => implicit connection =>
      SiteRequestLogic.reject(seq, adminSeq, reason)
    }
  }

  private def withAdminReview(seq: Long)(
    review: (Long, Option[String]) => java.sql.Connection => Either[SiteRequestLogic.SiteRequestError, SiteRequest]
  )(implicit request: Request[AnyContent]): Result = {
    if (!isAdmin) {
      AccessDenied
    } else {
      val reason = bodyField("rejectReason").map(_.take(rejectReasonMaxLength))
      SessionLogic.getUser(request) match {
        case None => JsonError(Unauthorized, "Login required.")
        case Some(admin) =>
          val result = database.withTransaction(connection => review(admin.seq, reason)(connection))
          // After the commit: see SiteRequestLogic.approve.
          AhaWikiCacheMemoryDomainSite.invalidate()
          result match {
            case Left(SiteRequestLogic.SiteRequestError.NotFound) => JsonError(NotFound, "Request not found.")
            case Left(error) => JsonError(BadRequest, error.message)
            case Right(row) => database.withConnection(implicit connection => Ok(requestJson(row, User.selectBySeq(row.user))))
          }
      }
    }
  }
}
