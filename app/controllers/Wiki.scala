package controllers

import java.net.URLDecoder
import java.net.URLEncoder
import java.util.Date
import java.net.URL

import actors.ActorAhaWiki.Calculate
import akka.actor._
import com.aha00a.commons.Implicits._
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
import logics.wikis.interpreters.Interpreters
import models.ContextSite.RequestWrapper
import models._
import models.tables.Page
import play.api.Configuration
import play.api.Environment
import play.api.Logging
import play.api.Mode
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
import logics.wikis.WikiSnippet
import models.tables.Site

class Wiki @Inject()(implicit val
                     controllerComponents: ControllerComponents,
                     actorSystem: ActorSystem,
                     database: Database,
                     environment: Environment,
                     @Named("db-actor") actorAhaWiki: ActorRef,
                     configuration: Configuration,
                     wsClient: WSClient,
                     executionContext: ExecutionContext
                    ) extends BaseController with Logging {

  implicit class RichResult(result: Result) {
    def withHeaderRobotNoIndexNoFollow: Result = result.withHeaders("X-Robots-Tag" -> "noindex, nofollow")
  }

  import java.sql.Connection

  import io.circe.generic.auto._
  import io.circe.syntax._
  def Ok(json: io.circe.Json): Result = Ok(json.toString()).as(JSON)

  def view(nameEncoded: String, revision: Int, action: String): Action[AnyContent] = Action { implicit request =>
    database.withConnection { implicit connection =>
      implicit val site: Site = Site.selectWhereDomain(request.host).getOrElse(Site(-1, ""))

      val name = URLDecoder.decode(nameEncoded.replace("+", "%2B"), "UTF-8")

      implicit val contextWikiPage: ContextWikiPage = ContextWikiPage(name)
      implicit val provider: RequestWrapper = contextWikiPage.requestWrapper

      val pageFirstRevision = Page.selectFirstRevision(name)
      val pageLastRevision = Page.selectLastRevision(name)
      val pageSpecificRevision = Page.select(name, revision)

      val pageLastRevisionContent = pageLastRevision.map(s => PageContent(s.content))
      val wikiPermission = WikiPermission()
      val isReadable = wikiPermission.isReadable(pageLastRevisionContent)
      val isWritable = wikiPermission.isWritable(pageLastRevisionContent)

      if(environment.mode == Mode.Dev) {
        import models.tables.Permission
//        val permissionLogic = new PermissionLogic(Permission.select()) // TODO:
        val permissionLogic = new PermissionLogic(Seq())
        val id = SessionLogic.getId(request).getOrElse("")
        val readable = permissionLogic.permitted(name, id, Permission.read)
        val editable = permissionLogic.permitted(name, id, Permission.edit)

        logger.error("Permission\t" + Seq(isReadable, readable, isWritable, editable).mkString("\t"))

        if(isReadable != readable || isWritable != editable)
          logger.error(s"readable: $readable editable: $editable")
      }

      //noinspection ScalaUnusedSymbol
      (pageSpecificRevision, action, isReadable, isWritable) match {
        case (None, "edit", _, true) =>
          val content = DefaultPageLogic.getOption(name).getOrElse(s"""= $name\n""")
          val page = Page(name, 0, new Date(), "AhaWiki", "127.0.0.1", "", "", content)
          Ok(views.html.Wiki.edit(page, ApplicationConf())).withHeaders("X-Robots-Tag" -> "noindex, nofollow")

        case (None, "edit", _, false) =>
          Forbidden(views.html.Wiki.error(name, "Permission denied.")).withHeaderRobotNoIndexNoFollow



        case (None, _, _, _) =>
          val additionalInfo = getAhaMarkAdditionalInfo(name)
          val regexSchemaColon: Regex = """^schema:(.+)$""".r
          DefaultPageLogic.getOption(name).map(content => {
            val contentInterpreted = Interpreters.toHtmlString(content + additionalInfo)
            Ok(views.html.Wiki.view(name, name, "", contentInterpreted, isWritable, pageFirstRevision, pageLastRevision))
          }).getOrElse({
            val content = WikiSnippet.notFound(name)
            val contentInterpreted = Interpreters.toHtmlString(content + additionalInfo)
            NotFound(views.html.Wiki.view(name, name, "", contentInterpreted, isWritable, pageFirstRevision, pageLastRevision))
          })

        case (Some(page), "" | "view", true, _) =>
          try {
            val pageContent: PageContent = PageContent(page.content)
            val additionalInfo = getAhaMarkAdditionalInfo(name)
            pageContent.redirect match {
              case Some(directive) =>
                val message = s"""Redirected from <a href="${page.name}?action=edit">${page.name}</a>"""
                val newMessage = request.flash.get("success").map(v => v + "<br/>" + message).getOrElse(message)
                Redirect(URLEncoder.encode(directive, "utf-8").replace("+", "%20")).flashing("success" -> newMessage)
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
              actorAhaWiki ! Calculate(site, name)
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

  def getAhaMarkAdditionalInfo(name: String)(implicit wikiContext: ContextWikiPage, connection: Connection, site: Site): String = {
    import models.tables.CosineSimilarity
    import models.tables.Link

    val markupSchema = getMarkupSchema(name)
    // TODO: refactoring
    val optionLink: Option[Link] = Link.selectDstLimit1(name)
    val seqCosineSimilarities: Seq[CosineSimilarity] = CosineSimilarity
      .select(name)
      .view
      .filter(_.and(wikiContext.pageCanSee))
      .take(1)
      .toSeq

    val markupSchemaWithTitle = markupSchema.toOption.map(s => s"=== [schema:Schema Schema]\n$s").getOrElse("")
    val markupBacklinksWithTitle = optionLink.map(s => s"=== Backlinks\n[[Backlinks]]").getOrElse("")
    val markupCosineSimilaritiesWithTitle = seqCosineSimilarities.headOption.map(s => s"=== Similar Pages\nSimilar pages by cosine similarity. Words after page name are term frequency.\n[[SimilarPages]]").getOrElse("")

    s"""
       |== See Also
       |$markupSchemaWithTitle
       |
       |$markupBacklinksWithTitle
       |
       |$markupCosineSimilaritiesWithTitle
       |
       |=== Adjacent Pages
       |[[AdjacentPages]]
       |
       |""".stripMargin
  }

  private def getMarkupSchema(name: String)(implicit wikiContext: ContextWikiPage, connection: Connection, site: Site) = {
    import models.tables.SchemaOrg
    val listSchemaOrg = SchemaOrg.selectWhereValue(name).filter(s => s.and(wikiContext.pageCanSee))
    val mapClsList = listSchemaOrg.groupBy(_.cls)
    mapClsList.keys.toSeq.sorted.map(k => {
      s"""==== [schema:$k $k]
         |[[Html(<div class="columnWidth350">)]]
         |${mapClsList(k).map(t => s""" * [schema:${t.prop} ${t.prop}] of ["${t.page}"]""").mkString("\n")}
         |[[Html(</div>)]]
         |""".stripMargin
    }).mkString("\n")
  }



  def save(nameEncoded: String): Action[AnyContent] = Action.async { implicit request =>
    val name = URLDecoder.decode(nameEncoded.replace("+", "%2B"), "UTF-8")

    val (revision, body, comment, minorEdit, recaptcha) = Form(tuple("revision" -> number, "text" -> text, "comment" -> text, "minorEdit" -> boolean, "recaptcha" -> text)).bindFromRequest.get
    val secretKey = ApplicationConf().AhaWiki.google.reCAPTCHA.secretKey()
    val remoteAddress = request.remoteAddressWithXRealIp

    def doSave() = {
      database.withConnection { implicit connection =>
        implicit val site: Site = Site.selectWhereDomain(request.host).getOrElse(Site(-1, ""))
        implicit val contextWikiPage: ContextWikiPage = ContextWikiPage(name)
        implicit val provider: RequestWrapper = contextWikiPage.requestWrapper
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
    val name = Form("name" -> text).bindFromRequest.get
    database.withTransaction { implicit connection =>
      implicit val site: Site = Site.selectWhereDomain(request.host).getOrElse(Site(-1, ""))
      implicit val contextWikiPage: ContextWikiPage = ContextWikiPage(name)
      implicit val provider: RequestWrapper = contextWikiPage.requestWrapper
      Page.selectLastRevision(name) match {
        case Some(page) =>
          if (WikiPermission().isWritable(PageContent(page.content))) {
            Page.deleteWithRelatedData(name)
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
      implicit val site: Site = Site.selectWhereDomain(request.host).getOrElse(Site(-1, ""))
      implicit val contextWikiPage: ContextWikiPage = ContextWikiPage(name)
      implicit val provider: RequestWrapper = contextWikiPage.requestWrapper
      Page.selectLastRevision(name) match {
        case Some(page) =>
          if (WikiPermission().isWritable(PageContent(page.content))) {
            Page.deleteSpecificRevisionWithRelatedData(name, page.revision)
            actorAhaWiki ! Calculate(site, name)
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
      implicit val site: Site = Site.selectWhereDomain(request.host).getOrElse(Site(-1, ""))
      val (pageName, url, sheetName) = Form(tuple("pageName" -> text, "url" -> text, "sheetName" -> text)).bindFromRequest.get
      Page.selectLastRevision(pageName) match {
        case Some(page) =>
          implicit val contextWikiPage: ContextWikiPage = ContextWikiPage(pageName)
          implicit val provider: RequestWrapper = contextWikiPage.requestWrapper
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
      implicit val site: Site = Site.selectWhereDomain(request.host).getOrElse(Site(-1, ""))
      val (name, newName) = Form(tuple("name" -> text, "newName" -> text)).bindFromRequest.get
      implicit val contextWikiPage: ContextWikiPage = ContextWikiPage(name)
      implicit val provider: RequestWrapper = contextWikiPage.requestWrapper
      (Page.selectLastRevision(name), Page.selectLastRevision(newName)) match {
        case (Some(page), None) =>
          if (WikiPermission().isWritable(PageContent(page.content))) {
            Page.rename(name, newName)
            PageLogic.insert(name, 1, new Date(), "redirect", s"#!redirect $newName")
            actorAhaWiki ! Calculate(site, newName)
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
    database.withConnection { implicit connection =>
      implicit val site: Site = Site.selectWhereDomain(request.host).getOrElse(Site(-1, ""))
      implicit val contextWikiPage: ContextWikiPage = ContextWikiPage.preview(name)
      val additionalInfo = getAhaMarkAdditionalInfo(name)
      Ok(s"""<div class="wikiContent preview"><div class="limitWidth">${Interpreters.toHtmlString(body + additionalInfo)}</div></div>""")
    }
  }

}





