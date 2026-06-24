package controllers

import com.aha00a.play.Implicits.RichRequest
import io.circe.Json
import io.circe.syntax._
import logics.AhaWikiCache
import logics.ApplicationConf
import logics.SessionLogic
import logics.SiteLogic
import logics.wikis.PageLogic
import logics.wikis.RenderingMode
import logics.wikis.WikiPermission
import models.ContextWikiPage
import models.PageContent
import models.RequestWrapper
import models.WikiActors
import models.tables.Page
import models.tables.Site
import models.tables.User
import play.api.Logging
import play.api.db.Database
import play.api.libs.json.{Json => PlayJson}
import play.api.mvc._

import java.net.URLDecoder
import java.time.LocalDateTime
import javax.inject._

class BotApi @Inject()(
  implicit val
  controllerComponents: ControllerComponents,
  database: Database,
  wikiActors: WikiActors,
  applicationConf: ApplicationConf,
  ahaWikiCache: AhaWikiCache,
) extends BaseController with Logging {

  private def Ok(json: io.circe.Json): Result = Results.Ok(json.toString()).as(JSON)

  private def decodePageName(nameEncoded: String): String =
    URLDecoder.decode(nameEncoded.replace("+", "%2B"), "UTF-8")

  private def unauthorized: Result =
    Unauthorized(Json.obj("error" -> Json.fromString("API key is required or invalid.")).toString()).as(JSON)

  private def withApiUser(request: RequestHeader)(f: User.SessionUser => Result): Result =
    SessionLogic.getApiKeyUser(request)(database).map(f).getOrElse(unauthorized)

  def getPage(nameEncoded: String): Action[AnyContent] = Action { implicit request =>
    withApiUser(request) { user =>
      val name = decodePageName(nameEncoded)
      database.withConnection { implicit connection =>
        implicit val site: Site = SiteLogic.get(request.host)
        implicit val requestWrapper: RequestWrapper = RequestWrapper.forUser(user)
        implicit val contextWikiPage: ContextWikiPage = new ContextWikiPage(Seq(name), RenderingMode.Normal)

        Page.selectLastRevision(name) match {
          case None =>
            NotFound(Json.obj("error" -> Json.fromString("Page not found.")).toString()).as(JSON)
          case Some(page) if !WikiPermission().isReadable(name, Some(PageContent(page.content))) =>
            Forbidden(Json.obj("error" -> Json.fromString("Permission denied.")).toString()).as(JSON)
          case Some(page) =>
            Ok(Json.obj(
              "name" -> Json.fromString(page.name),
              "revision" -> Json.fromLong(page.revision),
              "content" -> Json.fromString(page.content),
              "dateTime" -> Json.fromString(page.dateTime.toString),
              "viaApi" -> Json.fromBoolean(page.viaApi),
            ))
        }
      }
    }
  }

  def savePage(nameEncoded: String): Action[AnyContent] = Action { implicit request =>
    withApiUser(request) { user =>
      request.body.asJson match {
        case None =>
          BadRequest(Json.obj("error" -> Json.fromString("JSON body is required.")).toString()).as(JSON)
        case Some(body) =>
          val name = decodePageName(nameEncoded)
          val revisionOpt = (body \ "revision").asOpt[Long]
          val textOpt = (body \ "text").asOpt[String]
          val comment = (body \ "comment").asOpt[String].getOrElse("")
          val isMinorEdit = (body \ "minorEdit").asOpt[Boolean].getOrElse(false)

          (revisionOpt, textOpt) match {
            case (None, _) =>
              BadRequest(Json.obj("error" -> Json.fromString("revision is required.")).toString()).as(JSON)
            case (_, None) =>
              BadRequest(Json.obj("error" -> Json.fromString("text is required.")).toString()).as(JSON)
            case (Some(revision), Some(text)) =>
              database.withConnection { implicit connection =>
                implicit val site: Site = SiteLogic.get(request.host)
                implicit val requestWrapper: RequestWrapper = RequestWrapper.forUser(user)
                implicit val contextWikiPage: ContextWikiPage = new ContextWikiPage(Seq(name), RenderingMode.Normal)

                val latestPage = Page.selectLastRevision(name)
                val (latestText, latestRevision) = latestPage.map(page => (page.content, page.revision)).getOrElse(("", 0L))
                if (!WikiPermission().isWritable(name, latestPage.map(page => PageContent(page.content)))) {
                  Forbidden(Json.obj("error" -> Json.fromString("Permission denied.")).toString()).as(JSON)
                } else if (revision != latestRevision) {
                  Conflict(Json.obj(
                    "error" -> Json.fromString("revision != latestRevision"),
                    "latestRevision" -> Json.fromLong(latestRevision),
                  ).toString()).as(JSON)
                } else if (text == latestText) {
                  BadRequest(Json.obj("error" -> Json.fromString("text == latestText")).toString()).as(JSON)
                } else {
                  val now = LocalDateTime.now()
                  val nextRevision = latestRevision + 1
                  PageLogic.insert(name, nextRevision, now, comment, isMinorEdit, text, viaApi = true)
                  name match {
                    case ".footer" => ahaWikiCache.Footer.invalidate()
                    case ".config" => ahaWikiCache.Config.invalidate()
                    case _ => // do nothing
                  }
                  implicit val tupleDatabaseSite: (Database, Site) = (database, site)
                  ahaWikiCache.PageMeta.SeqPageLatestSummary.invalidate()
                  Ok(Json.obj(
                    "name" -> Json.fromString(name),
                    "revision" -> Json.fromLong(nextRevision),
                    "dateTime" -> Json.fromString(now.toString),
                  ))
                }
              }
          }
      }
    }
  }
}
