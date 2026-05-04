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
  private val CommentPreviewMaxLength = 80

  private case class AddListRequest(title: String, lineStart: Int)
  private implicit val addListRequestReads = Json.reads[AddListRequest]
  private case class AddCardRequest(text: String, lineStart: Int)
  private implicit val addCardRequestReads = Json.reads[AddCardRequest]
  private case class SaveKanbanRequest(lineStart: Int, lineEnd: Int, content: String)
  private implicit val saveKanbanRequestReads = Json.reads[SaveKanbanRequest]

  private def trimmedPreview(text: String, maxLength: Int = CommentPreviewMaxLength): String = {
    text.trim.take(maxLength)
  }

  private def buildKanbanActionComment(action: String,
                                       pageName: String,
                                       line: Int,
                                       detail: String,
                                       boardHint: Option[String] = None): String = {
    val safeDetail = trimmedPreview(detail)
    val boardScope = boardHint.map(value => s""" board="${trimmedPreview(value, 40)}"""").getOrElse("")

    s"""kanban:$action page=$pageName line=$line detail="$safeDetail"$boardScope"""
  }

  private def buildKanbanSaveComment(pageName: String,
                                     safeStart: Int,
                                     safeEndExclusive: Int,
                                     original: Seq[String],
                                     replacement: Seq[String]): String = {
    val beforeCards = original.count(_.trim.startsWith("* "))
    val afterCards = replacement.count(_.trim.startsWith("* "))
    val beforeLists = original.count(_.trim.startsWith("== "))
    val afterLists = replacement.count(_.trim.startsWith("== "))

    val cardDelta = afterCards - beforeCards
    val listDelta = afterLists - beforeLists

    val cardDeltaText = if (cardDelta == 0) "cards=0" else s"cards=${if (cardDelta > 0) "+" else ""}$cardDelta"
    val listDeltaText = if (listDelta == 0) "lists=0" else s"lists=${if (listDelta > 0) "+" else ""}$listDelta"

    val preview = replacement
      .map(_.trim)
      .find(_.nonEmpty)
      .orElse(original.map(_.trim).find(_.nonEmpty))
      .getOrElse("")
      .take(CommentPreviewMaxLength)

    val previewText = if (preview.nonEmpty) s""" preview="$preview"""" else ""

    s"kanban:save page=$pageName range=$safeStart-$safeEndExclusive replaced=${original.size}->${replacement.size} $cardDeltaText $listDeltaText$previewText"
  }

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

                val comment = buildKanbanActionComment(
                  action = "list:add",
                  pageName = pageName,
                  line = insertedLine,
                  detail = payload.title
                )
                PageLogic.insert(pageName, nextRevision, LocalDateTime.now(), comment, isMinorEdit = false, updated)

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

                val comment = buildKanbanActionComment(
                  action = "card:add",
                  pageName = pageName,
                  line = insertedLine,
                  detail = payload.text
                )
                PageLogic.insert(pageName, nextRevision, LocalDateTime.now(), comment, isMinorEdit = false, updated)

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

  def save(pageName: String): Action[AnyContent] = Action { implicit request =>
    request.body.asJson match {
      case Some(body) =>
        body.validate[SaveKanbanRequest].fold(
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
                val safeStart = Math.max(1, payload.lineStart)
                val safeEndExclusive = Math.max(safeStart, payload.lineEnd)
                val startIndex = Math.min(safeStart - 1, lines.length)
                val endIndex = Math.min(safeEndExclusive - 1, lines.length)
                val nextRevision = latest.map(_.revision + 1).getOrElse(1L)
                val replacement = payload.content.split("""\r\n|\n""", -1).toSeq
                val originalSlice = lines.slice(startIndex, endIndex).toSeq

                lines.remove(startIndex, Math.max(0, endIndex - startIndex))
                lines.insertAll(startIndex, replacement)

                val updated = lines.mkString("\n")
                val comment = buildKanbanSaveComment(pageName, safeStart, safeEndExclusive, originalSlice, replacement)
                PageLogic.insert(pageName, nextRevision, LocalDateTime.now(), comment, isMinorEdit = false, updated)

                Ok(Json.obj(
                  "status" -> "ok",
                  "message" -> "Kanban order saved.",
                  "pageName" -> pageName,
                  "lineStart" -> safeStart,
                  "lineEnd" -> (safeStart + replacement.size),
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
