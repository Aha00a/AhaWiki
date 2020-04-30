package controllers

import java.net.{URLDecoder, URLEncoder}
import java.time.{LocalDate, Month}
import java.util.Date

import actionCompositions.PostAction
import actors.ActorAhaWiki.Calculate
import akka.actor._
import com.aha00a.commons.Implicits._
import com.aha00a.commons.utils.{DateTimeUtil, RangeUtil}
import com.aha00a.play.Implicits._
import com.aha00a.play.utils.GoogleSpreadsheetApi
import com.aha00a.stemmers.Stemmer
import com.aha00a.supercsv.SupercsvUtil
import com.github.difflib.{DiffUtils, UnifiedDiffUtils}
import javax.inject.{Singleton, _}
import logics._
import logics.wikis.interpreters.InterpreterWiki.LinkMarkup
import logics.wikis.interpreters.Interpreters
import logics.wikis.macros.MacroMonthName
import logics.wikis.{ExtractConvertApplyChunkCustom, PageLogic, WikiPermission}
import models._
import org.apache.http.client.utils.URIUtils
import play.api.cache.CacheApi
import play.api.data.Form
import play.api.data.Forms._
import play.api.libs.json.JsValue
import play.api.libs.ws.WSClient
import play.api.libs.ws.WSResponse
import play.api.mvc._
import play.api.{Configuration, Environment, Logger, Mode}

import scala.collection.JavaConversions._
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.matching.Regex

// TODO: Why???? Singleton?
@Singleton
class Wiki @Inject()(implicit
                     cacheApi: CacheApi,
                     actorSystem: ActorSystem,
                     database: play.api.db.Database,
                     environment: Environment,
                     @Named("db-actor") actorAhaWiki: ActorRef,
                     configuration: Configuration,
                     ws: WSClient,
                     executor: ExecutionContext
                    ) extends Controller {

  implicit class RichResult(result:Result) {
    def withHeaderRobotNoIndexNoFollow: Result = result.withHeaders("X-Robots-Tag" -> "noindex, nofollow")
  }

  def view(nameEncoded: String, revision: Int, action: String): Action[AnyContent] = Action { implicit request => database.withConnection { implicit connection =>
    val name = URLDecoder.decode(nameEncoded.replaceAllLiterally("+", "%2B"), "UTF-8")
    implicit val wikiContext: WikiContext = WikiContext(name)

    val ahaWikiQuery: AhaWikiQuery = AhaWikiQuery()
    val pageFirstRevision = ahaWikiQuery.Page.selectFirstRevision(name)
    val pageLastRevision = ahaWikiQuery.Page.selectLastRevision(name)
    val pageSpecificRevision = ahaWikiQuery.Page.select(name, revision)

    val pageLastRevisionContent = pageLastRevision.map(s => PageContent(s.content))
    val wikiPermission = WikiPermission()
    val isWritable = wikiPermission.isWritable(pageLastRevisionContent)
    val isReadable = wikiPermission.isReadable(pageLastRevisionContent)

    //noinspection ScalaUnusedSymbol
    (pageSpecificRevision, action, isReadable, isWritable) match {
      case (None, "edit", _, true) =>
        val content = name match {
          case DateTimeUtil.regexIsoLocalDate(y, m, d) => s"[[DayHeader]]\n * "
          case _ => s"""= $name\n"""
        }
        Ok(views.html.Wiki.edit(models.Page(name, 0, new Date(), "AhaWiki", "127.0.0.1", "", "", content), ApplicationConf())).withHeaders("X-Robots-Tag" -> "noindex, nofollow")
      case (None, _, _, _) =>
        val additionalInfo = "\n== See Also\n[[SeeAlso]]\n"
        val regexSchemaColon: Regex = """^schema:$""".r

        name match {
          case DateTimeUtil.regexIsoLocalDate(y, m, d) =>
            val content =
              s"""[[DayHeader]]
                 |This page does not exist.
                 |== Possible actions
                 | * [[Html(<a href="?action=edit">create page</a>)]]
                 | * Search ["https://google.com/search?q=$name" $name] on Google
                 | * Search ["https://google.com/search?q=$name wiki" $name wiki] on Google
                 | * Search ["https://duckduckgo.com/?q=$name" $name] on DuckDuckGo
                 | * Search ["https://duckduckgo.com/?q=$name wiki" $name wiki] on DuckDuckGo
                 |""".stripMargin
            val contentInterpreted = Interpreters.interpret(content + additionalInfo)
            NotFound(views.html.Wiki.view(name, name, "", contentInterpreted, isWritable, pageFirstRevision, pageLastRevision))

          case DateTimeUtil.regexYearDashMonth(y, m) =>
            val localDate = LocalDate.of(y.toIntOrZero, Month.of(m.toIntOrZero), 1)
            val content =
              s"""= [[Html(${LinkMarkup(y).toHtmlString()})]]-$m
                 |[[[#!Html
                 |<div class="rightInfoBox">
                 |${RangeUtil.around(0, 12).map(i => LinkMarkup(localDate.plusMonths(i).toYearDashMonthString).toHtmlString()).mkString("<br/>")}
                 |</div>
                 |]]]
                 |[[IncludeDays]]
                 |""".stripMargin
            val contentInterpreted = Interpreters.interpret(content + additionalInfo)
            Ok(views.html.Wiki.view(name, name, "", contentInterpreted, isWritable, pageFirstRevision, pageLastRevision))

          case DateTimeUtil.regexYear(y) =>
            val content =
              s"""= $name
                 |[[[#!Html
                 |<div class="rightInfoBox">
                 |${RangeUtil.around(y.toInt, 10).map(y => LinkMarkup(y.toString).toHtmlString()).mkString("<br/>")}
                 |</div>
                 |]]]
                 |== Calendar
                 |${(1 to 12).map(m => f"[[Calendar($y-$m%02d)]]").mkString}
                 |""".stripMargin

            val contentInterpreted = Interpreters.interpret(content + additionalInfo)
            Ok(views.html.Wiki.view(name, name, "", contentInterpreted, isWritable, pageFirstRevision, pageLastRevision))

          case DateTimeUtil.regexDashDashMonthDashDay(mm, dd) =>
            val lastDay: Int = mm.toInt match {
              case 1 | 3 | 5 | 7 | 8 | 10 | 12 => 31
              case 4 | 6 | 9 | 11 => 30
              case 2 => 29
            }

            val r = <table class="month simpleTable">
              <thead>
                <tr>
                  <th colspan="31">{MacroMonthName(s"--$mm")}</th>
                </tr>
              </thead>
              <tbody>
                <tr>
                  {(1 to lastDay).grouped(5).map(t =>
                    <tr>
                      {t.map(Some(_)).padTo(5, None).map(d =>
                        <td>{d.map(d => scala.xml.XML.loadString(LinkMarkup(f"--$mm-$d%02d", f"$d%02d").toHtmlString())).getOrElse("")}</td>
                      )}
                    </tr>
                  )}
                </tr>
              </tbody>
            </table>
            val content =
              s"""= $mm-$dd
                 |[--$mm $mm]-[----$dd $dd]
                 |[[[#!Html
                 |${r.toString()}
                 |]]]
                 |""".stripMargin
            val contentInterpreted = Interpreters.interpret(content + additionalInfo)
            Ok(views.html.Wiki.view(name, name, "", contentInterpreted, isWritable, pageFirstRevision, pageLastRevision))


          case DateTimeUtil.regexDashDashMonth(mm) =>
            val lastDay: Int = mm.toInt match {
              case 1 | 3 | 5 | 7 | 8 | 10 | 12 => 31
              case 4 | 6 | 9 | 11 => 30
              case 2 => 29
            }

            val r = <table class="month simpleTable">
              <thead>
                <tr>
                  <th colspan="31">{MacroMonthName(s"--$mm")}</th>
                </tr>
              </thead>
              <tbody>
                <tr>
                  {(1 to lastDay).grouped(5).map(t =>
                    <tr>
                      {t.map(Some(_)).padTo(5, None).map(d =>
                        <td>{d.map(d => scala.xml.XML.loadString(LinkMarkup(f"--$mm-$d%02d", f"$d%02d").toHtmlString())).getOrElse("")}</td>
                      )}
                    </tr>
                  )}
                </tr>
              </tbody>
            </table>
            val content =
              s"""= [[MonthName]]
                 |[[[#!Html
                 |${r.toString()}
                 |]]]
                 |""".stripMargin
            val contentInterpreted = Interpreters.interpret(content + additionalInfo)
            Ok(views.html.Wiki.view(name, name, "", contentInterpreted, isWritable, pageFirstRevision, pageLastRevision))

//          case regexDashDashDashDashDay(mm) =>
//            Ok(mm) // TODO

//          case regexSchemaColon(schema) =>
//            Ok(schema) // TODO

          case _ => Ok(name)
            val content =
              s"""= $name
                 |This page does not exist.
                 |== Possible actions
                 | * [[Html(<a href="?action=edit">create page</a>)]]
                 | * Search ["https://google.com/search?q=$name" $name] on Google
                 | * Search ["https://google.com/search?q=$name wiki" $name wiki] on Google
                 | * Search ["https://duckduckgo.com/?q=$name" $name] on DuckDuckGo
                 | * Search ["https://duckduckgo.com/?q=$name wiki" $name wiki] on DuckDuckGo
                 |""".stripMargin
            val contentInterpreted = Interpreters.interpret(content + additionalInfo)
            NotFound(views.html.Wiki.view(name, name, "", contentInterpreted, isWritable, pageFirstRevision, pageLastRevision))
        }

      case (Some(page), "" | "view", true, _) =>
        try {
          val pageContent: PageContent = PageContent(page.content)
          val additionalInfo = "\n== See Also\n[[SeeAlso]]\n"
          pageContent.redirect match {
            case Some(directive) =>
              Redirect(URLEncoder.encode(directive, "utf-8").replaceAllLiterally("+", "%20")).flashing("success" -> s"""Redirected from <a href="${page.name}?action=edit">${page.name}</a>""")
            case None =>
              val description = pageContent.content.split("\n", 6).take(5).mkString("\n") + " ..."
              Ok(pageContent.interpreter match {
                case Some("Paper") =>
                  val contentInterpreted = Interpreters.interpret(page.content)
                  views.html.Wiki.view(name, description, "Paper", contentInterpreted, isWritable, pageFirstRevision, pageLastRevision)
                case None | Some("Wiki") =>
                  val contentInterpreted = Interpreters.interpret(page.content + additionalInfo)
                  if(request.isLocalhost) {
//                    Logger.info("Tw" + Stemmer.stemTwitter(page.content).sorted.mkString(","))
//                    Logger.info("EJ" + Stemmer.stemSeunjeon(page.content).sorted.mkString(","))
                    Logger.info("SP" + Stemmer.stemSplit(page.content).sorted.mkString(","))
                  }
                  views.html.Wiki.view(name, description, "Wiki", contentInterpreted, isWritable, pageFirstRevision, pageLastRevision)
                case _ =>
                  val contentInterpreted = s"""<h1>$name</h1>""" + Interpreters.interpret(page.content) + Interpreters.interpret(additionalInfo)
                  views.html.Wiki.view(name, description, pageContent.interpreter.getOrElse(""), contentInterpreted, isWritable, pageFirstRevision, pageLastRevision)
              })
          }
        }
        finally {
          if (environment.mode == Mode.Dev && request.isLocalhost)
            actorAhaWiki ! Calculate(name)
        }
      case (Some(page), "diff", true, _) =>
        val after = request.getQueryString("after").getOrElse("0").toInt
        val before = request.getQueryString("before").getOrElse((after - 1).toString).toInt

        val beforePage = ahaWikiQuery.Page.selectSpecificRevision(name, before)
        val afterPage = ahaWikiQuery.Page.selectSpecificRevision(name, after)

        val beforeContent = beforePage.map(_.content).getOrElse("").split("""(\r\n|\n)""").toSeq
        val afterContent = afterPage.map(_.content).getOrElse("").split("""(\r\n|\n)""").toSeq

        val beforeComment = beforePage.map(_.comment).getOrElse("")
        val afterComment = afterPage.map(_.comment).getOrElse("")

        val diff = DiffUtils.diff(beforeContent, afterContent)
        val unifiedDiff = UnifiedDiffUtils.generateUnifiedDiff(name, name, beforeContent, diff, 10).mkString("\n")
        Ok(views.html.Wiki.diff(name, before, beforeComment, after, afterComment, unifiedDiff)).withHeaders("X-Robots-Tag" -> "noindex, nofollow")

      case (Some(page), "raw", true, _) => Ok(page.content).withHeaderRobotNoIndexNoFollow
      case (Some(page), "history", true, _) => Ok(views.html.Wiki.history(name, ahaWikiQuery.Page.selectHistory(name))).withHeaderRobotNoIndexNoFollow
      case (Some(page), "blame", true, _) =>
        val blame = ahaWikiQuery.Page.selectHistoryStream(name, new Blame[PageMetaData, String](), (blame:Blame[PageMetaData, String], p) => blame.next(new PageMetaData(p), p.content.splitLinesSeq()))
        val seqRevision = blame.seqBlameLine.map(_.metaData.revision)
        val maxRevision: Long = seqRevision.max
        val minRevision: Long = seqRevision.min
        Ok(views.html.Wiki.blame(blame, minRevision, maxRevision, isWritable, pageFirstRevision, pageLastRevision)).withHeaderRobotNoIndexNoFollow

      case (Some(page), "edit", _, true) => Ok(views.html.Wiki.edit(page, ApplicationConf())).withHeaderRobotNoIndexNoFollow
      case (Some(page), "rename", _, true) => Ok(views.html.Wiki.rename(page)).withHeaderRobotNoIndexNoFollow
      case (Some(page), "delete", _, true) => Ok(views.html.Wiki.delete(page)).withHeaderRobotNoIndexNoFollow
      case _ => Forbidden(views.html.Wiki.error(name, "Permission denied.")).withHeaderRobotNoIndexNoFollow
    }
  }}

  def save(nameEncoded: String): Action[AnyContent] = PostAction.async { implicit request =>
    val name = URLDecoder.decode(nameEncoded.replaceAllLiterally("+", "%2B"), "UTF-8")
    implicit val wikiContext: WikiContext = WikiContext(name)

    val (revision, body, comment, minorEdit, recaptcha) = Form(tuple("revision" -> number, "text" -> text, "comment" -> text, "minorEdit" -> boolean, "recaptcha" -> text)).bindFromRequest.get
    val secretKey = ApplicationConf().AhaWiki.google.reCAPTCHA.secretKey()
    val remoteAddress = request.remoteAddressWithXRealIp

    def doSave() = {
      database.withConnection { implicit connection =>
        val (latestText, latestRevision, latestTime) = AhaWikiQuery().Page.selectLastRevision(name).map(w => (w.content, w.revision, w.dateTime)).getOrElse(("", 0, new Date()))
        if (!WikiPermission().isWritable(PageContent(latestText))) {
          Forbidden("")
        } else {
          if (revision != latestRevision) {
            Conflict("")
          } else {
            val now = new Date()
            val dateTime = if (minorEdit) latestTime else now
            val commentFixed = if (minorEdit) s"$comment - minor edit at ${now.toLocalDateTime.toIsoLocalDateTimeString}" else comment
            PageLogic.insert(name, revision + 1, dateTime, commentFixed, body)
            Ok("")
          }
        }
      }
    }

    if(secretKey != "") {
      ws.url("https://www.google.com/recaptcha/api/siteverify").post(Map("secret" -> Seq(secretKey), "response" -> Seq(recaptcha), "remoteip" -> Seq(remoteAddress))).map(response => {
        Logger.info(response.body)
        val json: JsValue = response.json
        if (!(json \ "success").as[Boolean]) {
          val errorCodes: Seq[String] = (json \ "error-codes").as[Seq[String]]
          Logger.error(s"robot - ${errorCodes.mkString("\t")}")
          Forbidden("")
        } else {
          doSave()
        }
      })
    } else {
      Future {
        doSave()
      }
    }
  }



  def delete(): Action[AnyContent] = PostAction { implicit request => database.withConnection { implicit connection =>
    val name = Form("name" -> text).bindFromRequest.get
    implicit val wikiContext: WikiContext = WikiContext(name)
    AhaWikiQuery().Page.selectLastRevision(name) match {
      case Some(page) =>
        if (WikiPermission().isWritable(PageContent(page.content))) {
          AhaWikiQuery().Page.deleteWithRelatedData(name)
          AhaWikiCache.PageList.invalidate()
          Ok("")
        } else {
          Forbidden("")
        }
      case None =>
        Forbidden("")
    }
  }}

  def deleteLastRevision(): Action[AnyContent] = PostAction { implicit request => database.withConnection { implicit connection =>
    val name = Form("name" -> text).bindFromRequest.get
    implicit val wikiContext: WikiContext = WikiContext(name)
    AhaWikiQuery().Page.selectLastRevision(name) match {
      case Some(page) =>
        if (WikiPermission().isWritable(PageContent(page.content))) {
          AhaWikiQuery().Page.deleteSpecificRevisionWithRelatedData(name, page.revision)
          AhaWikiCache.PageList.invalidate()
          actorAhaWiki ! Calculate(name)
          Ok("")
        } else {
          Forbidden("")
        }
      case None =>
        NotFound("")
    }
  }}

  val regexGoogleSpreadsheetUrl: Regex = """https://docs.google.com/spreadsheets/d/([^/]+)(/(edit(#gid=0)?)?)?""".r

  def padColumns[T](matrix: Seq[Seq[T]], default:T): Seq[Seq[T]] = {
    val maxLength = matrix.map(_.length).max
    matrix.map(_.padTo(maxLength, default))
  }

  def syncGoogleSpreadsheet: Action[AnyContent] = Action { implicit request => database.withConnection { implicit connection =>
    val (pageName, url, sheetName) = Form(tuple("pageName" -> text, "url" -> text, "sheetName" -> text)).bindFromRequest.get
    AhaWikiQuery().Page.selectLastRevision(pageName) match {
      case Some(page) =>
        implicit val wikiContext: WikiContext = WikiContext(pageName)
        val pageContent = PageContent(page.content)
        if (WikiPermission().isWritable(pageContent)) {
          val extractConvertApplyChunkRefresh = new ExtractConvertApplyChunkCustom(s => {
            val pageContentChunk = PageContent(s)
            if(url == pageContentChunk.argument.getOrElse(0, "") && sheetName == pageContentChunk.argument.getOrElse(1, "")) {
              url match {
                case regexGoogleSpreadsheetUrl(id, _, _, _) =>
                  val googleSheetsApiKey = ApplicationConf().AhaWiki.google.credentials.api.GoogleSheetsAPI.key()
                  val futureSpreadsheet: Future[Seq[Seq[String]]] = GoogleSpreadsheetApi.readSpreadSheet(googleSheetsApiKey, id, sheetName)
                  val spreadsheet: Seq[Seq[String]] = Await.result(futureSpreadsheet, 5 seconds)
                  s"[[[#!Map $url $sheetName\n${SupercsvUtil.toTsvString(padColumns(spreadsheet, ""))}]]]"
                case _ =>
                  s
              }
            } else {
              s
            }
          })
          val body = extractConvertApplyChunkRefresh(extractConvertApplyChunkRefresh.extract(pageContent.content))
          if (pageContent.content != body) {
            PageLogic.insert(pageName, page.revision + 1, new Date(), "Sync Google Spreadsheet", body)
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
  }}


  def rename(): Action[AnyContent] = PostAction { implicit request => database.withConnection { implicit connection =>
    val (name, newName) = Form(tuple("name" -> text, "newName" -> text)).bindFromRequest.get
    implicit val wikiContext: WikiContext = WikiContext(name)
    (AhaWikiQuery().Page.selectLastRevision(name), AhaWikiQuery().Page.selectLastRevision(newName)) match {
      case (Some(page), None) =>
        if (WikiPermission().isWritable(PageContent(page.content))) {
          AhaWikiQuery().Page.rename(name, newName)
          PageLogic.insert(name, 1, new Date(), "redirect", s"#!redirect $newName")
          AhaWikiCache.PageList.invalidate()
          actorAhaWiki ! Calculate(newName)
          Ok("")
        } else {
          Forbidden("")
        }
      case (Some(_), Some(_)) => Conflict("")
      case _ => Forbidden("")
    }
  }}


  def preview(): Action[AnyContent] = PostAction { implicit request =>
    val (name, body) = Form(tuple("name" -> text, "text" -> text)).bindFromRequest.get
    implicit val wikiContext: WikiContext = WikiContext.preview(name)
    Ok(s"""<div class="wikiContent preview"><div class="limitWidth">${Interpreters.interpret(body)}</div></div>""")
  }

}





