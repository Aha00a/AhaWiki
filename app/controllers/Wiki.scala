package controllers

import java.net.URLDecoder
import java.net.URLEncoder
import java.time.LocalDate
import java.time.Month
import java.util.Date

import actors.ActorAhaWiki.Calculate
import akka.actor._
import com.aha00a.commons.Implicits._
import com.aha00a.commons.utils.DateTimeUtil
import com.aha00a.commons.utils.RangeUtil
import com.aha00a.play.Implicits._
import com.aha00a.play.utils.GoogleSpreadsheetApi
import com.aha00a.supercsv.SupercsvUtil
import com.github.difflib.DiffUtils
import com.github.difflib.UnifiedDiffUtils
import javax.inject._
import logics._
import logics.wikis.ExtractConvertInjectInterpreterCustom
import logics.wikis.PageLogic
import logics.wikis.WikiPermission
import logics.wikis.interpreters.ahaMark.AhaMarkLink
import logics.wikis.interpreters.Interpreters
import logics.wikis.macros.MacroMonthName
import models.WikiContext.Provider
import models._
import models.tables.Page
import play.api.Configuration
import play.api.Environment
import play.api.Logging
import play.api.Mode
import play.api.cache.SyncCacheApi
import play.api.data.Form
import play.api.data.Forms._
import play.api.db.Database
import play.api.libs.json.JsValue
import play.api.libs.ws.WSClient
import play.api.mvc._

import scala.concurrent.Await
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.util.matching.Regex

class Wiki @Inject()(implicit val
                     controllerComponents: ControllerComponents,
                     syncCacheApi: SyncCacheApi,
                     actorSystem: ActorSystem,
                     database: Database,
                     environment: Environment,
                     @Named("db-actor") actorAhaWiki: ActorRef,
                     configuration: Configuration,
                     wsClient: WSClient,
                     executionContext: ExecutionContext
                    ) extends BaseController with Logging {
  implicit val ahaWikiInjects: AhaWikiInjects = AhaWikiInjects()

  implicit class RichResult(result: Result) {
    def withHeaderRobotNoIndexNoFollow: Result = result.withHeaders("X-Robots-Tag" -> "noindex, nofollow")
  }

  import io.circe.generic.auto._
  import io.circe.syntax._
  def Ok(json: io.circe.Json): Result = Ok(json.toString()).as(JSON)

  def view(nameEncoded: String, revision: Int, action: String): Action[AnyContent] = Action { implicit request =>
    database.withConnection { implicit connection =>
      import logics.wikis.WikiSnippet

      val name = URLDecoder.decode(nameEncoded.replace("+", "%2B"), "UTF-8")
      implicit val wikiContext: WikiContext = WikiContext(name)
      implicit val provider: Provider = wikiContext.provider

      val pageFirstRevision = Page.selectFirstRevision(name)
      val pageLastRevision = Page.selectLastRevision(name)
      val pageSpecificRevision = Page.select(name, revision)

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
          Ok(views.html.Wiki.edit(Page(name, 0, new Date(), "AhaWiki", "127.0.0.1", "", "", content), ApplicationConf())).withHeaders("X-Robots-Tag" -> "noindex, nofollow")
        case (None, _, _, _) =>
          val additionalInfo = "\n== See Also\n[[SeeAlso]]\n"
          val regexSchemaColon: Regex = """^schema:(.+)$""".r

          name match {
            case DateTimeUtil.regexIsoLocalDate(y, m, d) =>
              val content = WikiSnippet.notFoundWithDayHeader(name)
              val contentInterpreted = Interpreters.toHtmlString(content + additionalInfo)
              NotFound(views.html.Wiki.view(name, name, "", contentInterpreted, isWritable, pageFirstRevision, pageLastRevision))

            case DateTimeUtil.regexYearDashMonth(y, m) =>
              val localDate = LocalDate.of(y.toIntOrZero, Month.of(m.toIntOrZero), 1)
              val content =
                s"""= [[Html(${AhaMarkLink(y).toHtmlString()})]]-$m
                   |[[[#!Html
                   |<div class="rightInfoBox">
                   |${RangeUtil.around(0, 12).map(i => AhaMarkLink(localDate.plusMonths(i).toYearDashMonthString).toHtmlString()).mkString("<br/>")}
                   |</div>
                   |]]]
                   |[[IncludeDays]]
                   |""".stripMargin
              val contentInterpreted = Interpreters.toHtmlString(content + additionalInfo)
              Ok(views.html.Wiki.view(name, name, "", contentInterpreted, isWritable, pageFirstRevision, pageLastRevision))

            case DateTimeUtil.regexYear(y) =>
              val content =
                s"""= $name
                   |[[[#!Html
                   |<div class="rightInfoBox">
                   |${RangeUtil.around(y.toInt, 10).map(y => AhaMarkLink(y.toString).toHtmlString()).mkString("<br/>")}
                   |</div>
                   |]]]
                   |== Calendar
                   |${(1 to 12).map(m => f"[[Calendar($y-$m%02d)]]").mkString}
                   |""".stripMargin

              val contentInterpreted = Interpreters.toHtmlString(content + additionalInfo)
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
                    <th colspan="31">
                      {MacroMonthName.toHtmlString(s"--$mm")}
                    </th>
                  </tr>
                </thead>
                <tbody>
                  <tr>
                    {(1 to lastDay).grouped(5).map(t =>
                    <tr>
                      {t.map(Some(_)).padTo(5, None).map(d =>
                      <td>
                        {d.map(d => scala.xml.XML.loadString(AhaMarkLink(f"--$mm-$d%02d", f"$d%02d").toHtmlString())).getOrElse("")}
                      </td>
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
              val contentInterpreted = Interpreters.toHtmlString(content + additionalInfo)
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
                    <th colspan="31">
                      {MacroMonthName.toHtmlString(s"--$mm")}
                    </th>
                  </tr>
                </thead>
                <tbody>
                  <tr>
                    {(1 to lastDay).grouped(5).map(t =>
                    <tr>
                      {t.map(Some(_)).padTo(5, None).map(d =>
                      <td>
                        {d.map(d => scala.xml.XML.loadString(AhaMarkLink(f"--$mm-$d%02d", f"$d%02d").toHtmlString())).getOrElse("")}
                      </td>
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
              val contentInterpreted = Interpreters.toHtmlString(content + additionalInfo)
              Ok(views.html.Wiki.view(name, name, "", contentInterpreted, isWritable, pageFirstRevision, pageLastRevision))

            //          case regexDashDashDashDashDay(mm) =>
            //            Ok(mm) // TODO

            case "schema:Schema" =>
              import com.aha00a.commons.utils.EnglishCaseConverter
              val listSchemaOrg = models.tables.SchemaOrg.selectWhereProp("")
              val listSchemaOrgWithPermission = listSchemaOrg.filter(s => wikiContext.setPageNameByPermission.contains(s.page))
              val mapSchemaOrg = listSchemaOrgWithPermission.groupBy(_.cls)

              val content = s"""= Schema
                 |${listSchemaOrgWithPermission.size} page(s).
                 |${mapSchemaOrg.toSeq.sortBy(_._1).map(k =>
              s"""== ["schema:${k._1}" ${EnglishCaseConverter.pascalCase2TitleCase(k._1)}] (${k._2.size})
                 |${k._2.toSeq.map(_.page).map(s =>
              s""" * ["${s}"]""").mkString("\n")}
                 |""".stripMargin).mkString("\n")}
                 |""".stripMargin
              val contentInterpreted = Interpreters.toHtmlString(content + additionalInfo)
              NotFound(views.html.Wiki.view(name, name, "", contentInterpreted, isWritable, pageFirstRevision, pageLastRevision))

            case regexSchemaColon(schema) =>
              val optionSchemaType = SchemaOrg.mapAll.get(schema)
              optionSchemaType match {
                case Some(schemaType) =>
                  import models.tables.SchemaOrg
                  val content: String = if(schema(0).isUpper) {
                    val listSchemaOrg: List[SchemaOrg] = models.tables.SchemaOrg.selectWhereCls(schema)
                    s"""= ${schemaType.id}
                       |[[[#!Html
                       |${schemaType.comment}
                       |]]]
                       |${listSchemaOrg.map(s => s""" * ["${s.page}"]""").mkString("\n")}
                       |""".stripMargin
                  } else {
                    val listSchemaOrg: List[SchemaOrg] = models.tables.SchemaOrg.selectWhereProp(schema)
                    val listSchemaOrgWithPermission = listSchemaOrg.filter(s => wikiContext.setPageNameByPermission.contains(s.page))
                    s"""= ${schemaType.id}
                       |[[[#!Html
                       |${schemaType.comment}
                       |]]]
                       |${listSchemaOrgWithPermission.groupBy(_.value).transform((k, v) => v.groupBy(_.cls)).toSeq.sortBy(_._1).map(t =>
                    s"""== ["${t._1}" ${t._1}]
                       |${t._2.toSeq.sortBy(_._1).map(t2 =>
                    s"""=== ["schema:${t2._1}" ${t2._1}]
                       |${t2._2.map(s =>
                    s""" * ["${s.page}"]""").mkString("\n")}
                       |""".stripMargin).mkString("\n")}
                       |""".stripMargin).mkString("\n")}
                       |""".stripMargin
                  }

                  val contentInterpreted = Interpreters.toHtmlString(content + additionalInfo)
                  NotFound(views.html.Wiki.view(name, name, "", contentInterpreted, isWritable, pageFirstRevision, pageLastRevision))
                case _ =>
                  val content = WikiSnippet.notFound(name)
                  val contentInterpreted = Interpreters.toHtmlString(content + additionalInfo)
                  NotFound(views.html.Wiki.view(name, name, "", contentInterpreted, isWritable, pageFirstRevision, pageLastRevision))
              }

            case _ =>
              val content = WikiSnippet.notFound(name)
              val contentInterpreted = Interpreters.toHtmlString(content + additionalInfo)
              NotFound(views.html.Wiki.view(name, name, "", contentInterpreted, isWritable, pageFirstRevision, pageLastRevision))
          }

        case (Some(page), "" | "view", true, _) =>
          try {
            val pageContent: PageContent = PageContent(page.content)
            val additionalInfo = "\n== See Also\n[[SeeAlso]]\n"
            pageContent.redirect match {
              case Some(directive) =>
                Redirect(URLEncoder.encode(directive, "utf-8").replace("+", "%20")).flashing("success" -> s"""Redirected from <a href="${page.name}?action=edit">${page.name}</a>""")
              case None =>
                val description = pageContent.content.split("\n", 6).take(5).mkString("\n") + " ..."
                Ok(pageContent.interpreter match {
                  case Some("Paper") =>
                    val contentInterpreted = Interpreters.toHtmlString(page.content)
                    views.html.Wiki.view(name, description, "Paper", contentInterpreted, isWritable, pageFirstRevision, pageLastRevision)
                  case None | Some("Wiki") =>
                    val contentInterpreted = Interpreters.toHtmlString(page.content + additionalInfo)
                    views.html.Wiki.view(name, description, "Wiki", contentInterpreted, isWritable, pageFirstRevision, pageLastRevision)
                  case _ =>
                    val contentInterpreted = s"""<h1>$name</h1>""" + Interpreters.toHtmlString(page.content) + Interpreters.toHtmlString(additionalInfo)
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

          val beforePage = Page.selectSpecificRevision(name, before)
          val afterPage = Page.selectSpecificRevision(name, after)

          val beforeContent = beforePage.map(_.content).getOrElse("").split("""(\r\n|\n)""").toSeq
          val afterContent = afterPage.map(_.content).getOrElse("").split("""(\r\n|\n)""").toSeq

          val beforeComment = beforePage.map(_.comment).getOrElse("")
          val afterComment = afterPage.map(_.comment).getOrElse("")

          val diff = DiffUtils.diff(beforeContent.asJava, afterContent.asJava)
          val unifiedDiff = UnifiedDiffUtils.generateUnifiedDiff(name, name, beforeContent.asJava, diff, 10).asScala.mkString("\n")
          Ok(views.html.Wiki.diff(name, before, beforeComment, after, afterComment, unifiedDiff)).withHeaders("X-Robots-Tag" -> "noindex, nofollow")

        case (Some(page), "raw", true, _) => Ok(page.content).withHeaderRobotNoIndexNoFollow
        case (Some(page), "history", true, _) => Ok(views.html.Wiki.history(name, Page.selectHistory(name))).withHeaderRobotNoIndexNoFollow
        case (Some(page), "blame", true, _) =>
          import com.aha00a.colors.GradientPreset
          val blame = Page.selectHistoryStream(name, new Blame[PageMetaData, String](), (blame: Blame[PageMetaData, String], p) => blame.next(new PageMetaData(p), p.content.splitLinesSeq()))
          val seqRevision: Seq[Long] = blame.seqBlameLine.map(_.metaData.revision).distinct.sorted
          val mapRevisionColor = seqRevision.map(v => (v, GradientPreset.ahaWikiBlame.getColor(seqRevision.indexOf(v).toDouble / seqRevision.size).toHashString)).toMap
          Ok(views.html.Wiki.blame(blame, mapRevisionColor, isWritable, pageFirstRevision, pageLastRevision)).withHeaderRobotNoIndexNoFollow

        case (Some(page), "edit", _, true) => Ok(views.html.Wiki.edit(page, ApplicationConf())).withHeaderRobotNoIndexNoFollow
        case (Some(page), "rename", _, true) => Ok(views.html.Wiki.rename(page)).withHeaderRobotNoIndexNoFollow
        case (Some(page), "delete", _, true) => Ok(views.html.Wiki.delete(page)).withHeaderRobotNoIndexNoFollow
        case _ => Forbidden(views.html.Wiki.error(name, "Permission denied.")).withHeaderRobotNoIndexNoFollow
      }
    }
  }

  def save(nameEncoded: String): Action[AnyContent] = Action.async { implicit request =>
    val name = URLDecoder.decode(nameEncoded.replace("+", "%2B"), "UTF-8")
    implicit val wikiContext: WikiContext = WikiContext(name)
    implicit val provider: Provider = wikiContext.provider

    val (revision, body, comment, minorEdit, recaptcha) = Form(tuple("revision" -> number, "text" -> text, "comment" -> text, "minorEdit" -> boolean, "recaptcha" -> text)).bindFromRequest.get
    val secretKey = ApplicationConf().AhaWiki.google.reCAPTCHA.secretKey()
    val remoteAddress = request.remoteAddressWithXRealIp

    def doSave() = {
      database.withConnection { implicit connection =>
        val (latestText, latestRevision, latestTime) = Page.selectLastRevision(name).map(w => (w.content, w.revision, w.dateTime)).getOrElse(("", 0, new Date()))
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

    if (secretKey != "") {
      wsClient.url("https://www.google.com/recaptcha/api/siteverify").post(Map("secret" -> Seq(secretKey), "response" -> Seq(recaptcha), "remoteip" -> Seq(remoteAddress))).map(response => {
        logger.info(response.body)
        val json: JsValue = response.json
        if (!(json \ "success").as[Boolean]) {
          val errorCodes: Seq[String] = (json \ "error-codes").as[Seq[String]]
          logger.error(s"robot - ${errorCodes.mkString("\t")}")
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


  def delete(): Action[AnyContent] = Action { implicit request =>
    database.withConnection { implicit connection =>
      val name = Form("name" -> text).bindFromRequest.get
      implicit val wikiContext: WikiContext = WikiContext(name)
      implicit val provider: Provider = wikiContext.provider
      Page.selectLastRevision(name) match {
        case Some(page) =>
          if (WikiPermission().isWritable(PageContent(page.content))) {
            Page.deleteWithRelatedData(name)
            AhaWikiCache.PageList.invalidate()
            Ok("")
          } else {
            Forbidden("")
          }
        case None =>
          Forbidden("")
      }
    }
  }

  def deleteLastRevision(): Action[AnyContent] = Action { implicit request =>
    database.withConnection { implicit connection =>
      val name = Form("name" -> text).bindFromRequest.get
      implicit val wikiContext: WikiContext = WikiContext(name)
      implicit val provider: Provider = wikiContext.provider
      Page.selectLastRevision(name) match {
        case Some(page) =>
          if (WikiPermission().isWritable(PageContent(page.content))) {
            Page.deleteSpecificRevisionWithRelatedData(name, page.revision)
            AhaWikiCache.PageList.invalidate()
            actorAhaWiki ! Calculate(name)
            Ok("")
          } else {
            Forbidden("")
          }
        case None =>
          NotFound("")
      }
    }
  }

  val regexGoogleSpreadsheetUrl: Regex = """https://docs.google.com/spreadsheets/d/([^/]+)(/(edit(#gid=0)?)?)?""".r

  def padColumns[T](matrix: Seq[Seq[T]], default: T): Seq[Seq[T]] = {
    val maxLength = matrix.map(_.length).max
    matrix.map(_.padTo(maxLength, default))
  }

  def syncGoogleSpreadsheet: Action[AnyContent] = Action { implicit request =>
    database.withConnection { implicit connection =>
      val (pageName, url, sheetName) = Form(tuple("pageName" -> text, "url" -> text, "sheetName" -> text)).bindFromRequest.get
      Page.selectLastRevision(pageName) match {
        case Some(page) =>
          implicit val wikiContext: WikiContext = WikiContext(pageName)
          implicit val provider: Provider = wikiContext.provider
          val pageContent = PageContent(page.content)
          if (WikiPermission().isWritable(pageContent)) {
            val extractConvertApplyInterpreterRefresh = new ExtractConvertInjectInterpreterCustom(s => {
              val pageContentChunk = PageContent(s)
              if (url == pageContentChunk.argument.getOrElse(0, "") && sheetName == pageContentChunk.argument.getOrElse(1, "")) {
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
            val body = extractConvertApplyInterpreterRefresh.inject(extractConvertApplyInterpreterRefresh.extract(pageContent.content))
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
    }
  }


  def rename(): Action[AnyContent] = Action { implicit request =>
    database.withConnection { implicit connection =>
      val (name, newName) = Form(tuple("name" -> text, "newName" -> text)).bindFromRequest.get
      implicit val wikiContext: WikiContext = WikiContext(name)
      implicit val provider: Provider = wikiContext.provider
      (Page.selectLastRevision(name), Page.selectLastRevision(newName)) match {
        case (Some(page), None) =>
          if (WikiPermission().isWritable(PageContent(page.content))) {
            Page.rename(name, newName)
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
    }
  }


  def preview(): Action[AnyContent] = Action { implicit request =>
    val (name, body) = Form(tuple("name" -> text, "text" -> text)).bindFromRequest.get
    implicit val wikiContext: WikiContext = WikiContext.preview(name)
    Ok(s"""<div class="wikiContent preview"><div class="limitWidth">${Interpreters.toHtmlString(body)}</div></div>""")
  }

}





