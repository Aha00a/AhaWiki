package controllers

import java.net.URLDecoder
import java.time.LocalDate
import java.time.format.TextStyle
import java.util.Locale
import javax.inject.{Singleton, _}

import actionCompositions.PostAction
import actors.ActorPageProcessor
import actors.ActorPageProcessor.Calculate
import akka.actor._
import com.aha00a.commons.implicits.Implicits._
import com.aha00a.commons.utils._
import difflib.DiffRowGenerator
import logics._
import logics.wikis.{Interpreters, WikiPermission}
import models.Database.Link
import models.{Database, MockDb, PageContent, WikiContext}
import play.api.cache.CacheApi
import play.api.data.Form
import play.api.data.Forms._
import play.api.mvc._

import scala.collection.JavaConversions._

@Singleton
class Wiki @Inject()(implicit cacheApi: CacheApi, actorSystem: ActorSystem) extends Controller {

  def view(nameEncoded: String, revision: Int, action: String) = Action { implicit request =>
    val name = URLDecoder.decode(nameEncoded.replaceAllLiterally("+",  "%2B"), "UTF-8")
    implicit val wikiContext = WikiContext(name)

    val pageFirstRevision = MockDb.selectPageFirstRevision(name)
    val pageLastRevision = MockDb.selectPageLastRevision(name)
    val pageSpecificRevision = MockDb.selectPage(name, revision)

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
        Ok(views.html.Wiki.edit(new models.Database.Page(name, content))).withHeaders("X-Robots-Tag" -> "noindex, nofollow")
      case (None, _, _, _) =>
        val relatedPages = getRelatedPages(name)
        val additionalInfo =
          s"""= $name
             |This page does not exist. You can [[Html(<a href="?action=edit">create</a>)]] it here.
             |= See also
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
              val similarPages = getSimilarPages(name)
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
          if (play.Play.isDev && request.isLocalhost)
            actorSimilarPage ! Calculate(name)
        }
      case (Some(page), "diff", true, _) =>
        val after = request.getQueryString("after").getOrElse("0").toInt
        val before = request.getQueryString("before").getOrElse((after-1).toString).toInt

        val afterPage = MockDb.selectPageSpecificRevision(name, after)
        val beforePage = MockDb.selectPageSpecificRevision(name, before)

        val afterContent = afterPage.map(_.content).getOrElse("").split( """(\r\n|\n)""").toSeq
        val beforeContent = beforePage.map(_.content).getOrElse("").split( """(\r\n|\n)""").toSeq

        val listDiffRow = new DiffRowGenerator.Builder().ignoreBlankLines(false).ignoreWhiteSpaces(false).build().generateDiffRows(beforeContent, afterContent)

        Ok(views.html.Wiki.diff(name, before, after, listDiffRow)).withHeaders("X-Robots-Tag" -> "noindex, nofollow")
      case (Some(page), "raw", true, _) => Ok(page.content).withHeaders("X-Robots-Tag" -> "noindex, nofollow")
      case (Some(page), "history", true, _) => Ok(views.html.Wiki.history(name, Database.pageSelectHistory(name))).withHeaders("X-Robots-Tag" -> "noindex, nofollow")
      case (Some(page), "edit", _, true) => Ok(views.html.Wiki.edit(page)).withHeaders("X-Robots-Tag" -> "noindex, nofollow")
      case (Some(page), "rename", _, true) => Ok(views.html.Wiki.rename(page)).withHeaders("X-Robots-Tag" -> "noindex, nofollow")
      case (Some(page), "delete", _, true) => Ok(views.html.Wiki.delete(page)).withHeaders("X-Robots-Tag" -> "noindex, nofollow")
      case _ => Forbidden(views.html.Wiki.error(name, "Permission denied.")).withHeaders("X-Robots-Tag" -> "noindex, nofollow")
    }
  }


  def getSimilarPages(name: String): String = {
    val similarPages = Database.cosineSimilaritySelect(name)
      .map(c => s" * [[LinkWithPercent(${c.name2},${"%2.2f".format(c.similarity * 100)}%)]]")
      .mkString("\n")

    if (similarPages != "") {
      similarPages
    }
    else {
      ""
    }
  }


  def getRelatedPages(name: String): String = {
    val relationship: List[Link] = Database.linkSelect(name)
    val back: List[Link] = relationship.flatMap(lm => Database.linkSelect(lm.src))
    val forward: List[Link] = relationship.flatMap(lm => Database.linkSelect(lm.dst))

    val result = (relationship ++ back ++ forward)
      .map(l => s"${l.src}->${l.dst}")
      .mkString("\n")

    if (result != "") {
      s"""[[[#!Graph
         |#!enableWikiLink
         |$result
         |]]]""".stripMargin
    }
    else {
      ""
    }
  }

  val actorSimilarPage: ActorRef = actorSystem.actorOf(ActorPageProcessor.props)

  def save(nameEncoded: String) = PostAction { implicit request =>
    val name = URLDecoder.decode(nameEncoded.replaceAllLiterally("+",  "%2B"), "UTF-8")
    implicit val wikiContext = WikiContext(name)

    val (revision, body, comment) = Form(tuple("revision" -> number, "text" -> text, "comment" -> text)).bindFromRequest.get
    val (latestText, latestRevision) = MockDb.selectPageLastRevision(name).map(w => (w.content, w.revision)).getOrElse(("", 0))

    if (WikiPermission.isWritable(PageContent(latestText))) {
      if (revision == latestRevision) {
        Database.pageInsert(name, revision + 1, DateTimeUtil.nowEpochNano, SessionLogic.getId(request).getOrElse("anonymous"), request.remoteAddressWithXRealIp, body, comment)
        actorSimilarPage ! Calculate(name)

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
    MockDb.selectPageLastRevision(name) match {
      case Some(page) =>
        if (WikiPermission.isWritable(PageContent(page.content))) {
          Database.pageDeleteWithRelatedData(name)
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
    MockDb.selectPageLastRevision(name) match {
      case Some(page) =>
        if (WikiPermission.isWritable(PageContent(page.content))) {
          Database.pageDeleteRevisionWithRelatedData(name, page.revision)
          Cache.PageList.invalidate()
          actorSimilarPage ! Calculate(name)
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
    (MockDb.selectPageLastRevision(name), MockDb.selectPageLastRevision(newName)) match {
      case (Some(page), None) =>
        if (WikiPermission.isWritable(PageContent(page.content))) {
          Database.pageRename(name, newName)
          Database.pageInsert(name, 1, DateTimeUtil.nowEpochNano, SessionLogic.getId(request).getOrElse("anonymous"), request.remoteAddressWithXRealIp, s"#!redirect $newName", "redirect")
          Cache.PageList.invalidate()
          actorSimilarPage ! Calculate(newName)
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
    implicit val wikiContext = WikiContext(name)
    Ok( """<div class="limitWidth"><div class="wikiContent preview">""" + Interpreters.interpret(body) + """</div></div>""")
  }

}





