package controllers

import java.net.URLDecoder
import java.sql.Connection
import java.time.format.TextStyle
import java.time.{LocalDate, LocalDateTime}
import java.util.{Date, Locale}

import actionCompositions.PostAction
import actors.ActorAhaWiki.Calculate
import akka.actor._
import com.aha00a.commons.Implicits._
import com.aha00a.commons.utils.RangeUtil
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
        val additionalInfo =
          s"""
             |== See also
             |[[Html(<table class="seeAlso"><thead><tr><th>Page Suggestion</th><th>Related Pages</th></tr></thead><tbody><tr><td>)]]
             |'''[schema:Schema Schema]'''
             |${getMarkupSchema(name, ahaWikiQuery)}
             |'''Backlinks'''
             |[[Backlinks]]
             |[[Html(</td><td>)]]
             |${getMarkupRelatedPages(name)}
             |[[Html(</td></tr></tbody></table>)]]
             |""".stripMargin

        val regexPageType: Regex ="""(?x)
            ~~~~~~~~~~~~~~~~~~~~~~
          | (\d{4})
          | (\d{4}-\d\d)
          | (\d\d-\d\d)
          | (\d{4}-\d\d-\d\d)
          | schema:(.+)
          | (.+)
        """.r
        name match {
          case regexPageType(y   , null, null, null, null  , null) =>
            val content =
              s"""= $name
                 |[[[#!Table tsv 1
                 |${(RangeUtil.around(0, 5)).map(y => f"'''$y%+d'''").mkString("\t")}
                 |${(RangeUtil.around(y.toInt, 5)).map(y => s"[$y]").mkString("\t")}
                 |]]]
                 |== Calendar
                 |${(1 to 12).map(m => f"[[Calendar($y-$m%02d)]]").mkString}
                 |""".stripMargin

            val contentInterpreted = Interpreters.interpret(content + additionalInfo)
            Ok(views.html.Wiki.view(name, name, contentInterpreted, isWritable, pageFirstRevision, pageLastRevision))
          case regexPageType(null, ym  , null, null, null  , null) => Ok(ym)
          case regexPageType(null, null, md  , null, null  , null) => Ok(md)
          case regexPageType(null, null, null, ymd , null  , null) => Ok(ymd)
          case regexPageType(null, null, null, null, schema, null) => Ok(schema)
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

            NotFound(views.html.Wiki.notFound(name, Interpreters.interpret(content + additionalInfo)))
        }

      case (Some(page), "" | "view", true, _) =>
        try {
          val pageContent: PageContent = PageContent(page.content)
          pageContent.redirect match {
            case Some(directive) =>
              Redirect(directive).flashing("success" -> s"""Redirected from <a href="${page.name}?action=edit">${page.name}</a>""")
            case None =>
              val additionalInfo =
                s"""
                   |== See also
                   |[[Html(<table class="seeAlso"><thead><tr><th>Page Suggestion</th><th>Related Pages</th></tr></thead><tbody><tr><td>)]]
                   |'''[schema:Schema Schema]'''
                   |${getMarkupSchema(name, ahaWikiQuery)}
                   |'''Similar Pages'''
                   |${getMarkupSimilarPages(name, ahaWikiQuery)}
                   |'''Backlinks'''
                   |[[Backlinks]]
                   |[[Html(</td><td>)]]
                   |${getMarkupRelatedPages(name)}
                   |[[Html(</td></tr></tbody></table>)]]
                   |""".stripMargin
              val description = pageContent.content.split("\n", 6).take(5).mkString("\n") + " ..."
              Ok(pageContent.interpreter match {
                case Some("Paper") =>
                  val contentInterpreted = Interpreters.interpret(page.content)
                  views.html.Wiki.view(name, description, contentInterpreted, isWritable, pageFirstRevision, pageLastRevision)
                case None | Some("Wiki") =>
                  val contentInterpreted = Interpreters.interpret(page.content + additionalInfo) 
                  if(request.isLocalhost) {
//                    Logger.info("Tw" + Stemmer.stemTwitter(page.content).sorted.mkString(","))
//                    Logger.info("EJ" + Stemmer.stemSeunjeon(page.content).sorted.mkString(","))
                    Logger.info("SP" + Stemmer.stemSplit(page.content).sorted.mkString(","))
                  }
                  views.html.Wiki.view(name, description, contentInterpreted, isWritable, pageFirstRevision, pageLastRevision)
                case _ =>
                  val contentInterpreted = s"""<h1>$name</h1>""" + Interpreters.interpret(page.content) + Interpreters.interpret(additionalInfo)
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

  private def getMarkupSimilarPages(name: String, ahaWikiQuery: AhaWikiQuery) = {
    val cosineSimilarities: immutable.Seq[CosineSimilarity] = ahaWikiQuery.CosineSimilarity.select(name)
    if (cosineSimilarities.isEmpty) {
      actorAhaWiki ! Calculate(name)
    }
    val similarPageNames = cosineSimilarities.map(_.name2)
    val highScoredTerms = ahaWikiQuery.selectHighScoredTerm(name, similarPageNames).groupBy(_.name).mapValues(_.map(_.term).mkString(", "))
    val similarPages = cosineSimilarities.map(c => s""" * [[PercentLinkTitle(${c.similarity}, ${c.name2}, "${highScoredTerms.getOrElse(c.name2, "")}")]]""").mkString("\n")
    similarPages
  }

  private def getMarkupSchema(name: String, ahaWikiQuery: AhaWikiQuery) = {
    val seqLinkSchema: List[Link] = ahaWikiQuery.Link.selectSchema(name)
    val mapClassSrcProperty: Map[String, List[(String, String, String)]] = seqLinkSchema.map(l => {
      val splitted = l.alias.split(":")
      splitted match {
        case Array(s0, s1) => (l.src, s1, "")
        case Array(s0, s1, s2) => (l.src, s1, s2)
      }
    }).filter(_._3.isNotNullOrEmpty).groupBy(_._2)
    mapClassSrcProperty.keys.toSeq.sorted.map(k => {
      s""" * [schema:$k $k]
         |${mapClassSrcProperty(k).map(t => s"""  * [schema:${t._3} ${t._3}] of ["${t._1}"]""").mkString("\n")}""".stripMargin
    }).mkString("\n")
  }

  def getMarkupRelatedPages(name: String)(implicit connection: Connection): String = {
    val ahaWikiQuery: AhaWikiQuery = AhaWikiQuery()
    val seqLink: Seq[Link] = ahaWikiQuery.Link.select(name)
    val seqLinkExpanded: scala.Seq[_root_.models.Link] = ahaWikiQuery.Link.expand(seqLink)
    val result = seqLinkExpanded
      .map(l => s"${l.src}->${l.dst}")
      .mkString("\n")

    result.toOption.map(r => {
      s"""[[[#!Graph enableWikiLink
         |$result
         |]]]
         |""".stripMargin
    }).getOrElse("")
  }
  
  def save(nameEncoded: String): Action[AnyContent] = PostAction { implicit request => database.withConnection { implicit connection =>
    val name = URLDecoder.decode(nameEncoded.replaceAllLiterally("+", "%2B"), "UTF-8")
    implicit val wikiContext: WikiContext = WikiContext(name)

    val (revision, body, comment, minorEdit) = Form(tuple("revision" -> number, "text" -> text, "comment" -> text, "minorEdit" -> boolean)).bindFromRequest.get
    val (latestText, latestRevision, latestTime) = AhaWikiQuery().Page.selectLastRevision(name).map(w => (w.content, w.revision, w.dateTime)).getOrElse(("", 0, new Date()))

    if (WikiPermission().isWritable(PageContent(latestText))) {
      if (revision == latestRevision) {
        val now = new Date()
        pageInsertLogic(request, name, revision + 1, if(minorEdit) latestTime else now,  body, if(minorEdit) s"$comment - minor edit at ${now.toLocalDateTime.toIsoLocalDateTimeString}" else comment)
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





