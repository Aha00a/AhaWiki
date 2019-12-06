package controllers

import java.net.URLDecoder
import java.time.LocalDate
import java.time.format.TextStyle
import java.util.Locale

import actionCompositions.PostAction
import actors.ActorAhaWiki.Calculate
import akka.actor._
import com.aha00a.commons.implicits.Implicits._
import com.aha00a.commons.utils._
import com.github.difflib.{DiffUtils, UnifiedDiffUtils}
import javax.inject.{Singleton, _}
import logics._
import logics.wikis.{Interpreters, WikiPermission}
import models.{AhaWikiDatabase, PageContent, WikiContext}
import play.api.cache.CacheApi
import play.api.data.Form
import play.api.data.Forms._
import play.api.mvc._
import play.api.{Environment, Mode}

import scala.collection.JavaConversions._
import scala.collection.immutable

//noinspection TypeAnnotation
@Singleton
class Wiki @Inject()(
  implicit cacheApi: CacheApi,
  actorSystem: ActorSystem,
  database:play.api.db.Database,
  environment: Environment,
  @Named("db-actor") actorAhaWiki: ActorRef
) extends Controller {
  private val ahaWikiDatabase = AhaWikiDatabase()

  def view(nameEncoded: String, revision: Int, action: String) = Action { implicit request =>
    val name = URLDecoder.decode(nameEncoded.replaceAllLiterally("+",  "%2B"), "UTF-8")
    implicit val wikiContext: WikiContext = WikiContext(name)

    val pageFirstRevision = ahaWikiDatabase.pageSelectFirstRevision(name)
    val pageLastRevision = ahaWikiDatabase.pageSelectLastRevision(name)
    val pageSpecificRevision = ahaWikiDatabase.pageSelect(name, revision)

    val pageLastRevisionContent = pageLastRevision.map(s => PageContent(s.content))
    val isWritable = WikiPermission.isWritable(pageLastRevisionContent)
    val isReadable = WikiPermission.isReadable(pageLastRevisionContent)

    //noinspection ScalaUnusedSymbol
    (pageSpecificRevision, action, isReadable, isWritable) match {
      case (None, "edit", _, true) =>
        val regexYmd = """^(\d{4})-(\d{2})-(\d{2})$""".r
        val content = name match {
          case regexYmd(y, m, d) =>
            val localDate = LocalDate.parse(name)
            val weekdayName = localDate.getDayOfWeek.getDisplayName(TextStyle.SHORT, Locale.KOREAN)
            s"= [$y-$m]-$d $weekdayName\n * "
          case _ => s"""= $name\ndescribe $name here."""
        }
        Ok(views.html.Wiki.edit(new models.AhaWikiDatabase.Page(name, 0, DateTimeUtil.nowEpochNano, "AhaWiki", "127.0.0.1", content, None))).withHeaders("X-Robots-Tag" -> "noindex, nofollow")
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
              val cosineSimilarities: immutable.Seq[AhaWikiDatabase.CosineSimilarity] = ahaWikiDatabase.cosineSimilaritySelect(name)
              val similarPageNames = cosineSimilarities.map(_.name2)
              val highScoredTerms = ahaWikiDatabase.selectHighScoredTerm(name, similarPageNames).groupBy(_.name).mapValues(_.map(_.term).mkString(", "))
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
        val before = request.getQueryString("before").getOrElse((after-1).toString).toInt

        val beforePage = ahaWikiDatabase.pageSelectSpecificRevision(name, before)
        val afterPage = ahaWikiDatabase.pageSelectSpecificRevision(name, after)

        val beforeContent = beforePage.map(_.content).getOrElse("").split( """(\r\n|\n)""").toSeq
        val afterContent = afterPage.map(_.content).getOrElse("").split( """(\r\n|\n)""").toSeq

        val diff = DiffUtils.diff(beforeContent, afterContent)
        val unifiedDiff = UnifiedDiffUtils.generateUnifiedDiff(name, name, beforeContent, diff, 10).mkString("\n")
        Ok(views.html.Wiki.diff(name, before, after, unifiedDiff)).withHeaders("X-Robots-Tag" -> "noindex, nofollow")

      case (Some(page), "raw", true, _) => Ok(page.content).withHeaders("X-Robots-Tag" -> "noindex, nofollow")
      case (Some(page), "history", true, _) => Ok(views.html.Wiki.history(name, ahaWikiDatabase.pageSelectHistory(name))).withHeaders("X-Robots-Tag" -> "noindex, nofollow")
      case (Some(page), "edit", _, true) => Ok(views.html.Wiki.edit(page)).withHeaders("X-Robots-Tag" -> "noindex, nofollow")
      case (Some(page), "rename", _, true) => Ok(views.html.Wiki.rename(page)).withHeaders("X-Robots-Tag" -> "noindex, nofollow")
      case (Some(page), "delete", _, true) => Ok(views.html.Wiki.delete(page)).withHeaders("X-Robots-Tag" -> "noindex, nofollow")
      case _ => Forbidden(views.html.Wiki.error(name, "Permission denied.")).withHeaders("X-Robots-Tag" -> "noindex, nofollow")
    }
  }

  def getRelatedPages(name: String): String = {
    val relationship = ahaWikiDatabase.linkSelect(name)
    val backward = relationship.flatMap(lm => ahaWikiDatabase.linkSelect(lm.src))
    val forward = relationship.flatMap(lm => ahaWikiDatabase.linkSelect(lm.dst))

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




  def save(nameEncoded: String) = PostAction { implicit request =>
    val name = URLDecoder.decode(nameEncoded.replaceAllLiterally("+",  "%2B"), "UTF-8")
    implicit val wikiContext: WikiContext = WikiContext(name)

    val (revision, body, comment) = Form(tuple("revision" -> number, "text" -> text, "comment" -> text)).bindFromRequest.get
    val (latestText, latestRevision) = ahaWikiDatabase.pageSelectLastRevision(name).map(w => (w.content, w.revision)).getOrElse(("", 0))

    if (WikiPermission.isWritable(PageContent(latestText))) {
      if (revision == latestRevision) {
        ahaWikiDatabase.pageInsert(name, revision + 1, DateTimeUtil.nowEpochNano, SessionLogic.getId(request).getOrElse("anonymous"), request.remoteAddressWithXRealIp, body, comment)
        actorAhaWiki ! Calculate(name)

        Cache.PageList.invalidate()
        name match {
          case ".header" => Cache.Header.invalidate()
          case ".footer" => Cache.Footer.invalidate()
          case ".config" => Cache.Config.invalidate()
          case _ =>
        }

        Ok("")
      } else {
        Conflict("")
      }
    } else {
      Forbidden("")
    }
  }

  def delete() = PostAction { implicit request =>
    val name = Form("name" -> text).bindFromRequest.get
    implicit val wikiContext: WikiContext = WikiContext(name)
    ahaWikiDatabase.pageSelectLastRevision(name) match {
      case Some(page) =>
        if (WikiPermission.isWritable(PageContent(page.content))) {
          ahaWikiDatabase.pageDeleteWithRelatedData(name)
          Cache.PageList.invalidate()
          Ok("")
        } else {
          Forbidden("")
        }
      case None =>
        Forbidden("")
    }
  }

  def deleteLastRevision() = PostAction { implicit request =>
    val name = Form("name" -> text).bindFromRequest.get
    implicit val wikiContext: WikiContext = WikiContext(name)
    ahaWikiDatabase.pageSelectLastRevision(name) match {
      case Some(page) =>
        if (WikiPermission.isWritable(PageContent(page.content))) {
          ahaWikiDatabase.pageDeleteRevisionWithRelatedData(name, page.revision)
          Cache.PageList.invalidate()
          actorAhaWiki ! Calculate(name)
          Ok("")
        } else {
          Forbidden("")
        }
      case None =>
        Forbidden("")
    }
  }

  def rename() = PostAction { implicit request =>
    val (name, newName) = Form(tuple("name" -> text, "newName" -> text)).bindFromRequest.get
    implicit val wikiContext: WikiContext = WikiContext(name)
    (ahaWikiDatabase.pageSelectLastRevision(name), ahaWikiDatabase.pageSelectLastRevision(newName)) match {
      case (Some(page), None) =>
        if (WikiPermission.isWritable(PageContent(page.content))) {
          ahaWikiDatabase.pageRename(name, newName)
          ahaWikiDatabase.pageInsert(name, 1, DateTimeUtil.nowEpochNano, SessionLogic.getId(request).getOrElse("anonymous"), request.remoteAddressWithXRealIp, s"#!redirect $newName", "redirect")
          Cache.PageList.invalidate()
          actorAhaWiki ! Calculate(newName)
          Ok("")
        } else {
          Forbidden("")
        }
      case (Some(_), Some(_)) => Conflict("")
      case _ => Forbidden("")
    }
  }


  def preview() = PostAction { implicit request =>
    val (name, body) = Form(tuple("name" -> text, "text" -> text)).bindFromRequest.get
    implicit val wikiContext: WikiContext = WikiContext(name)
    Ok( """<div class="limitWidth"><div class="wikiContent preview">""" + Interpreters.interpret(body) + """</div></div>""")
  }

}





