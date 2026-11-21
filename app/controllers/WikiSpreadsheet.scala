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
  crossInstanceBus: CrossInstanceBus,
) extends BaseController with FormResults with Logging {

  val regexGoogleSpreadsheetUrl: Regex = WikiSpreadsheet.regexGoogleSpreadsheetUrl

  def padColumns[T](matrix: Seq[Seq[T]], default: T): Seq[Seq[T]] = {
    val maxLength = matrix.map(_.length).max
    matrix.map(_.padTo(maxLength, default))
  }

  def syncGoogleSpreadsheet: Action[AnyContent] = Action { implicit request => withForm(Form(tuple("pageName" -> text, "url" -> text, "sheetName" -> text))) { case (pageName, url, sheetName) =>
    database.withConnection { implicit connection =>
      implicit val site: Site = SiteLogic.get(request.host)
      Page.selectLastRevision(pageName) match {
        case Some(page) =>
          implicit val contextWikiPage: ContextWikiPage = ContextWikiPage(pageName).holding(connection)
          implicit val provider: RequestWrapper = contextWikiPage.requestWrapper
          val pageContent = PageContent(page.content)
          if (WikiPermission().isWritable(pageName, pageExists = true)) {
            def fetchTsv(id: String): String = {
              val googleSheetsApiKey = applicationConf.AhaWiki.google.credentials.api.GoogleSheetsAPI.key()
              val spreadsheet = Await.result(GoogleSpreadsheetApi.readSpreadSheet(googleSheetsApiKey, id, sheetName), 5 seconds)
              SupercsvUtil.toTsvString(padColumns(spreadsheet, ""))
            }
            // Case 1: embedded [[[#!Map url sheetName\ncontent]]] form
            val updatedBodyContent = WikiSpreadsheet.syncEmbeddedBlocks(pageContent.content, url, sheetName, fetchTsv)
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
              val nextRevision = page.revision + 1
              val dateInserted = LocalDateTime.now()
              val editorNickname = provider.getUser.map(_.nickname).getOrElse("Guest")
              PageLogic.insert(pageName, nextRevision, dateInserted, "Sync Google Spreadsheet", isMinorEdit = false, newPageBody)
              // The table on the page just changed under whoever is reading it.
              crossInstanceBus.publishPageUpdated(site.seq, pageName, nextRevision, editorNickname, dateInserted, None)
              telegramLogic.notifySpreadsheetSynced(request.host, pageName, editorNickname, Config.Query.Telegram.chatId())
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
  }}
}

object WikiSpreadsheet {
  val regexGoogleSpreadsheetUrl: Regex = """https://docs\.google\.com/spreadsheets/d/([^/?#\s]+).*""".r

  /**
   * `content` with the body of every `[[[#!Map url sheetName ...]]]` block for this sheet replaced
   * by what `fetchTsv` gives for the sheet's id, and every other block left exactly as written.
   *
   * Every `[[[ ... ]]]` block of the page passes through here, because that is how the blocks are
   * found (ExtractConvertInject keeps only what is between the brackets). Until 2026-11-21 a block
   * that was not this sheet's came back without its brackets: one press of the map's sync button
   * saved the page with every other block -- a #!Vim listing, a #!Table, a second map -- turned into
   * loose text, `#!Vim` line and all. Putting the brackets back is the other half of the round trip
   * (MacroInlineDays.oneLevelDeeper does the same).
   */
  def syncEmbeddedBlocks(content: String, url: String, sheetName: String, fetchTsv: String => String)(implicit wikiContext: ContextWikiPage): String = {
    val extractor = ExtractConvertInject.markedBlocks(block => {
      val chunk = PageContent(block)
      val isThisSheet = url == chunk.argument.getOrElse(0, "") && sheetName == chunk.argument.getOrElse(1, "")
      (isThisSheet, url) match {
        case (true, regexGoogleSpreadsheetUrl(id)) =>
          val mapHeader = if (sheetName.nonEmpty) s"#!Map $url $sheetName" else s"#!Map $url"
          s"[[[$mapHeader\n${fetchTsv(id)}]]]"
        case _ =>
          s"[[[$block]]]"
      }
    })
    extractor.inject(extractor.extract(content))
  }
}
