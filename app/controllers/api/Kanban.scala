package controllers.api

import akka.actor.ActorRef
import akka.actor.ActorSystem
import logics.AhaWikiCache
import logics.ApplicationConf

import javax.inject._
import logics.SiteLogic
import logics.wikis.PageLogic
import logics.wikis.WikiPermission
import models.RequestWrapper
import models.{ContextWikiPage, PageContent}
import models.tables.Page
import models.tables.Site
import play.api.Configuration
import play.api.Environment
import play.api.libs.json.{JsError, Json}
import play.api.db.Database
import play.api.libs.ws.WSClient
import play.api.mvc._

import java.time.LocalDateTime
import scala.concurrent.ExecutionContext

@Singleton
class Kanban @Inject()(implicit val
controllerComponents: ControllerComponents,
                       actorSystem: ActorSystem,
                       database: Database,
                       environment: Environment,
                       @Named("db-actor") actorAhaWiki: ActorRef,
                       applicationConf: ApplicationConf,
                       ahaWikiCache: AhaWikiCache,
                       wsClient: WSClient,
                       executionContext: ExecutionContext,
                       configuration: Configuration) extends BaseController {
  private case class AddListRequest(title: String, lineStart: Int)
  private implicit val addListRequestReads = Json.reads[AddListRequest]
  private case class AddCardRequest(text: String, lineStart: Int)
  private implicit val addCardRequestReads = Json.reads[AddCardRequest]

  def listAdd(pageName: String): Action[AnyContent] = Action { implicit request =>
    request.body.asJson match {
      case Some(body) =>
        body.validate[AddListRequest].fold(
          errors => {
            BadRequest(
              Json.obj(
                "status" -> "error",
                "message" -> "Invalid payload",
                "errors" -> JsError.toJson(errors)
              )
            )
          },
          payload => {
            database.withConnection { implicit connection =>
              implicit val site: Site = SiteLogic.get(request.host)
              implicit val wikiContext: ContextWikiPage = ContextWikiPage(pageName)
              implicit val provider: RequestWrapper = wikiContext.requestWrapper

              val latest = Page.selectLastRevision(pageName)
              val latestContent = latest.map(_.content).getOrElse("")
              if (!WikiPermission().isWritable(PageContent(latestContent))) {
                Forbidden(Json.obj("status" -> "error", "message" -> "Permission denied."))
              } else {
                val lines = latestContent.split("""\r\n|\n""", -1).toBuffer
                val insertAt = Math.max(0, Math.min(payload.lineStart - 1, lines.length))
                val insertedLine = insertAt + 1
                lines.insert(insertAt, s"== ${payload.title}")
                val updated = lines.mkString("\n")
                val nextRevision = latest.map(_.revision + 1).getOrElse(1L)

                PageLogic.insert(pageName, nextRevision, LocalDateTime.now(), "add kanban list", isMinorEdit = false, updated)

                Ok(
                  Json.obj(
                    "status" -> "ok",
                    "message" -> "Kanban list saved.",
                    "pageName" -> pageName,
                    "title" -> payload.title,
                    "lineStart" -> insertedLine,
                    "lineEnd" -> insertedLine,
                    "revision" -> nextRevision
                  )
                )
              }
            }
          }
        )
      case None =>
        BadRequest(
          Json.obj(
            "status" -> "error",
            "message" -> "JSON body is required"
          )
        )
    }
  }

  def cardAdd(pageName: String): Action[AnyContent] = Action { implicit request =>
    request.body.asJson match {
      case Some(body) =>
        body.validate[AddCardRequest].fold(
          errors => {
            BadRequest(Json.obj("status" -> "error", "message" -> "Invalid payload", "errors" -> JsError.toJson(errors)))
          },
          payload => {
            database.withConnection { implicit connection =>
              implicit val site: Site = SiteLogic.get(request.host)
              implicit val wikiContext: ContextWikiPage = ContextWikiPage(pageName)
              implicit val provider: RequestWrapper = wikiContext.requestWrapper

              val latest = Page.selectLastRevision(pageName)
              val latestContent = latest.map(_.content).getOrElse("")
              if (!WikiPermission().isWritable(PageContent(latestContent))) {
                Forbidden(Json.obj("status" -> "error", "message" -> "Permission denied."))
              } else {
                val lines = latestContent.split("""\r\n|\n""", -1).toBuffer
                val insertAt = Math.max(0, Math.min(payload.lineStart - 1, lines.length))
                val insertedLine = insertAt + 1
                lines.insert(insertAt, s" * ${payload.text}")
                val updated = lines.mkString("\n")
                val nextRevision = latest.map(_.revision + 1).getOrElse(1L)

                PageLogic.insert(pageName, nextRevision, LocalDateTime.now(), "add kanban card", isMinorEdit = false, updated)

                Ok(Json.obj(
                  "status" -> "ok",
                  "message" -> "Kanban card saved.",
                  "pageName" -> pageName,
                  "text" -> payload.text,
                  "lineStart" -> insertedLine,
                  "lineEnd" -> insertedLine,
                  "revision" -> nextRevision
                ))
              }
            }
          }
        )
      case None =>
        BadRequest(Json.obj("status" -> "error", "message" -> "JSON body is required"))
    }
  }
}
