package controllers

import com.aha00a.commons.Implicits._
import com.aha00a.play.Implicits._
import com.aha00a.play.utils.GoogleSpreadsheetApi
import com.aha00a.supercsv.SupercsvUtil
import logics._
import logics.wikis.ExtractConvertInject
import logics.wikis.PageLogic
import logics.wikis.WikiPermission
import models.RequestWrapper
import models._
import models.tables.Config
import models.tables.Page
import models.tables.Site
import play.api.Logging
import play.api.data.Form
import play.api.data.Forms._
import play.api.db.Database
import play.api.libs.ws.WSClient
import play.api.mvc._

import java.time.LocalDateTime
import javax.inject._
import scala.concurrent.Await
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.util.matching.Regex

/**
 * Pulling a Google Spreadsheet into a page as a table.
 *
 * Kept apart from page editing because it is an integration with someone else's service: it
 * needs an API key, it can fail for reasons that have nothing to do with the wiki, and it
 * writes a page revision as a side effect of a fetch rather than of someone typing.
 */
class WikiSpreadsheet @Inject()(
  implicit val
  controllerComponents: ControllerComponents,
  database: Database,
  wikiActors: WikiActors,
  applicationConf: ApplicationConf,
  ahaWikiCache: AhaWikiCache,
  wsClient: WSClient,
  executionContext: ExecutionContext,
  telegramLogic: TelegramLogic,
) extends BaseController with Logging {

  val regexGoogleSpreadsheetUrl: Regex = """https://docs\.google\.com/spreadsheets/d/([^/?#\s]+).*""".r

  def padColumns[T](matrix: Seq[Seq[T]], default: T): Seq[Seq[T]] = {
    val maxLength = matrix.map(_.length).max
    matrix.map(_.padTo(maxLength, default))
  }

  def syncGoogleSpreadsheet: Action[AnyContent] = Action { implicit request =>
    database.withConnection { implicit connection =>
      implicit val site: Site = SiteLogic.get(request.host)
      val (pageName, url, sheetName) = Form(tuple("pageName" -> text, "url" -> text, "sheetName" -> text)).bindFromRequest().get
      Page.selectLastRevision(pageName) match {
        case Some(page) =>
          implicit val contextWikiPage: ContextWikiPage = ContextWikiPage(pageName)
          implicit val provider: RequestWrapper = contextWikiPage.requestWrapper
          val pageContent = PageContent(page.content)
          if (WikiPermission().isWritable(pageName, pageContent)) {
            def fetchTsv(id: String): String = {
              val googleSheetsApiKey = applicationConf.AhaWiki.google.credentials.api.GoogleSheetsAPI.key()
              val spreadsheet = Await.result(GoogleSpreadsheetApi.readSpreadSheet(googleSheetsApiKey, id, sheetName), 5 seconds)
              SupercsvUtil.toTsvString(padColumns(spreadsheet, ""))
            }
            // Case 1: embedded [[[#!Map url sheetName\ncontent]]] form
            val extractor = ExtractConvertInject.markedBlocks(s => {
              val chunk = PageContent(s)
              if (url == chunk.argument.getOrElse(0, "") && sheetName == chunk.argument.getOrElse(1, "")) {
                url match {
                  case regexGoogleSpreadsheetUrl(id) =>
                  val mapHeader = if (sheetName.nonEmpty) s"#!Map $url $sheetName" else s"#!Map $url"
                  s"[[[$mapHeader\n${fetchTsv(id)}]]]"
                  case _ => s
                }
              } else {
                s
              }
            })
            val updatedBodyContent = extractor.inject(extractor.extract(pageContent.content))
            val newPageBody = if (updatedBodyContent != pageContent.content) {
              // embedded form updated — reconstruct full page preserving page-level directives
              if (pageContent.directives.isEmpty) updatedBodyContent
              else pageContent.directives.map("#!" + _).mkString("\n") + "\n" + updatedBodyContent
            } else if (pageContent.interpreter.contains("Map") &&
                       url == pageContent.argument.getOrElse(0, "") &&
                       sheetName == pageContent.argument.getOrElse(1, "")) {
              // Case 2: page-level #!Map directive form
              url match {
                case regexGoogleSpreadsheetUrl(id) =>
                  pageContent.directives.map("#!" + _).mkString("\n") + "\n" + fetchTsv(id)
                case _ => page.content
              }
            } else {
              page.content
            }
            if (page.content != newPageBody) {
              PageLogic.insert(pageName, page.revision + 1, LocalDateTime.now(), "Sync Google Spreadsheet", isMinorEdit = false, newPageBody)
              telegramLogic.notifySpreadsheetSynced(request.host, pageName, provider.getUser.map(_.nickname).getOrElse("Guest"), Config.Query.Telegram.chatId())
              Ok("")
            } else {
              Ok("NotChanged")
            }
          } else {
            Forbidden("")
          }
        case None =>
          NotFound("")
      }
    }
  }
}
