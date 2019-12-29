package controllers

import java.net.URLDecoder
import java.sql.Connection
import java.time.LocalDate
import java.time.format.TextStyle
import java.util.{Date, Locale}

import actionCompositions.PostAction
import actors.ActorAhaWiki.Calculate
import akka.actor._
import com.aha00a.commons.Implicits._
import com.aha00a.play.Implicits._
import com.aha00a.play.utils.GoogleSpreadsheetApi
import com.aha00a.stemmers.Stemmer
import com.aha00a.supercsv.SupercsvUtil
import com.github.difflib.{DiffUtils, UnifiedDiffUtils}
import javax.inject.{Singleton, _}
import logics._
import logics.wikis.interpreters.Interpreters
import logics.wikis.{ExtractConvertApplyChunkCustom, WikiPermission}
import models._
import play.api.cache.CacheApi
import play.api.data.Form
import play.api.data.Forms._
import play.api.libs.ws.WSClient
import play.api.mvc._
import play.api.{Configuration, Environment, Logger, Mode}

import scala.collection.JavaConversions._
import scala.collection.immutable
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
  private val regexYmd: Regex = """^(\d{4})-(\d{2})-(\d{2})$""".r

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
          case regexYmd(y, m, d) =>
            val localDate = LocalDate.parse(name)
            val weekdayName = localDate.getDayOfWeek.getDisplayName(TextStyle.SHORT, Locale.KOREAN)
            s"= [$y-$m]-$d $weekdayName\n * "
          case _ => s"""= $name\ndescribe $name here."""
        }
        Ok(views.html.Wiki.edit(models.Page(name, 0, new Date(), "AhaWiki", "127.0.0.1", content, ""))).withHeaders("X-Robots-Tag" -> "noindex, nofollow")
      case (None, _, _, _) =>
        val relatedPages = getRelatedPages(name)
        val additionalInfo =
          s"""= $name
             |This page does not exist.
             |== Possible actions
             | * [[Html(<a href="?action=edit">create page</a>)]]
             | * Search [https://google.com/search?q=$name $name] on Google
             | * Search [https://google.com/search?q=$name+wiki $name wiki] on Google
             | * Search [https://duckduckgo.com/?q=$name $name] on DuckDuckGo
             | * Search [https://duckduckgo.com/?q=$name+wiki $name wiki] on DuckDuckGo
             |
             |== See also
             |[[Html(<table class="seeAlso"><tr><th>Similar Pages</th><th>Related Pages</th></tr><tr><td class="">)]]
             |'''Backlinks'''
             |[[Backlinks]][[Html(</td><td class="">)]]$relatedPages[[Html(</td></tr></table>)]]
             |""".stripMargin

        NotFound(views.html.Wiki.notFound(name, Interpreters.interpret(additionalInfo)))

      case (Some(page), "" | "view", true, _) =>
        try {
          val pageContent: PageContent = PageContent(page.content)
          pageContent.redirect match {
            case Some(directive) =>
              Redirect(directive).flashing("success" -> s"""Redirected from <a href="${page.name}?action=edit">${page.name}</a>""")
            case None =>
              val cosineSimilarities: immutable.Seq[CosineSimilarity] = ahaWikiQuery.CosineSimilarity.select(name)
              if(cosineSimilarities.isEmpty) {
                actorAhaWiki ! Calculate(name)
              }
              val similarPageNames = cosineSimilarities.map(_.name2)
              val highScoredTerms = ahaWikiQuery.selectHighScoredTerm(name, similarPageNames).groupBy(_.name).mapValues(_.map(_.term).mkString(", "))
              val similarPages = cosineSimilarities.map(c => " * [[[#!Html\n" + views.html.Wiki.percentLinkTitle(c.similarity, c.name2, highScoredTerms.getOrElse(c.name2, "")) + "\n]]]").mkString("\n")
              val relatedPages = getRelatedPages(name)
              val additionalInfo =
                s"""
                   |== See also
                   |[[Html(<table class="seeAlso"><tr><th>Page Suggestion</th><th>Related Pages</th></tr><tr><td class="">)]]
                   |'''SimilarPages'''
                   |$similarPages
                   |'''Backlinks'''
                   |[[Backlinks]][[Html(</td><td class="">)]]$relatedPages[[Html(</td></tr></table>)]]
                   |""".stripMargin
              val description = pageContent.content.split("\n", 6).take(5).mkString("\n") + " ..."
              Ok(pageContent.interpreter match {
                case Some("Paper") =>
                  val contentInterpreted = Interpreters.interpret(page.content)
                  views.html.Wiki.view(name, description, contentInterpreted, isWritable, pageFirstRevision, pageLastRevision)
                case None | Some("Wiki") =>
                  val contentInterpreted = """<div class="limitWidth"><div class="wikiContent">""" + Interpreters.interpret(page.content + additionalInfo) + """</div></div>"""
                  if(request.isLocalhost) {
//                    Logger.info("Tw" + Stemmer.stemTwitter(page.content).sorted.mkString(","))
//                    Logger.info("EJ" + Stemmer.stemSeunjeon(page.content).sorted.mkString(","))
                    Logger.info("SP" + Stemmer.stemSplit(page.content).sorted.mkString(","))
                  }
                  views.html.Wiki.view(name, description, contentInterpreted, isWritable, pageFirstRevision, pageLastRevision)
                case _ =>
                  val contentInterpreted = s"""<div class="limitWidth"><div class="wikiContent"><h1>$name</h1>""" + Interpreters.interpret(page.content) + Interpreters.interpret(additionalInfo) + """</div></div>"""
                  views.html.Wiki.view(name, description, contentInterpreted, isWritable, pageFirstRevision, pageLastRevision)
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
        Ok(views.html.Wiki.blame(blame, minRevision, maxRevision)).withHeaderRobotNoIndexNoFollow
        
      case (Some(page), "edit", _, true) => Ok(views.html.Wiki.edit(page)).withHeaderRobotNoIndexNoFollow
      case (Some(page), "rename", _, true) => Ok(views.html.Wiki.rename(page)).withHeaderRobotNoIndexNoFollow
      case (Some(page), "delete", _, true) => Ok(views.html.Wiki.delete(page)).withHeaderRobotNoIndexNoFollow
      case _ => Forbidden(views.html.Wiki.error(name, "Permission denied.")).withHeaderRobotNoIndexNoFollow
    }
  }}

  def getRelatedPages(name: String)(implicit connection: Connection): String = {
    val relationship = AhaWikiQuery().Link.linkSelect(name)
    val backward = relationship.flatMap(lm => AhaWikiQuery().Link.linkSelect(lm.src))
    val forward = relationship.flatMap(lm => AhaWikiQuery().Link.linkSelect(lm.dst))

    val result = (relationship ++ backward ++ forward)
      .map(l => s"${l.src}->${l.dst}")
      .mkString("\n")

    if (result != "") {
      s"[[[#!Graph enableWikiLink\n$result\n]]]"
    }
    else {
      ""
    }
  }


  def save(nameEncoded: String): Action[AnyContent] = PostAction { implicit request => database.withConnection { implicit connection =>
    val name = URLDecoder.decode(nameEncoded.replaceAllLiterally("+", "%2B"), "UTF-8")
    implicit val wikiContext: WikiContext = WikiContext(name)

    val (revision, body, comment, minorEdit) = Form(tuple("revision" -> number, "text" -> text, "comment" -> text, "minorEdit" -> boolean)).bindFromRequest.get
    val (latestText, latestRevision, latestTime) = AhaWikiQuery().Page.selectLastRevision(name).map(w => (w.content, w.revision, w.dateTime)).getOrElse(("", 0, new Date()))

    if (WikiPermission().isWritable(PageContent(latestText))) {
      if (revision == latestRevision) {
        pageInsertLogic(request, name, revision + 1, if(minorEdit) latestTime else new Date(),  body, comment)
        Ok("")
      } else {
        Conflict("")
      }
    } else {
      Forbidden("")
    }
  }}

  private def pageInsertLogic(request: Request[AnyContent], name: String, revision: Long, dateTime: Date, body: String, comment: String)(implicit wikiContext: WikiContext): Unit = { wikiContext.database.withConnection { implicit connection =>
    AhaWikiQuery().pageInsert(
      name,
      revision,
      dateTime,
      SessionLogic.getId(request).getOrElse("anonymous"),
      request.remoteAddressWithXRealIp,
      body,
      comment)

    actorAhaWiki ! Calculate(name)

    AhaWikiCache.PageList.invalidate()
    name match {
      case ".header" => AhaWikiCache.Header.invalidate()
      case ".footer" => AhaWikiCache.Footer.invalidate()
      case ".config" => AhaWikiCache.Config.invalidate()
      case _ =>
    }
  }}

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
            pageInsertLogic(request, pageName, page.revision + 1, new Date(), body, "Sync Google Spreadsheet")
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
          AhaWikiQuery().pageInsert(name, 1, new Date(), SessionLogic.getId(request).getOrElse("anonymous"), request.remoteAddressWithXRealIp, s"#!redirect $newName", "redirect")
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
    implicit val wikiContext: WikiContext = WikiContext(name, isPreview = true)
    Ok("""<div class="limitWidth"><div class="wikiContent preview">""" + Interpreters.interpret(body) + """</div></div>""")
  }

}





