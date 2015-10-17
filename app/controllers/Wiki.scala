package controllers

import java.net.URLDecoder
import javax.inject.{Singleton, _}

import actionCompositions.PostAction
import actors.ActorPageProcessor
import actors.ActorPageProcessor.Calculate
import akka.actor._
import logics._
import logics.wikis.{Interpreters, RecentlyVisited, WikiPermission}
import models.DirectQuery.Link
import models.{DirectQuery, MockDb, PageContent, WikiContext}
import play.api.data.Form
import play.api.data.Forms._
import play.api.mvc._
import utils._

@Singleton
class Wiki @Inject()(system: ActorSystem) extends Controller {


  def view(nameEncoded: String, revision: Int, action: String) = Action { implicit request =>
    val name = URLDecoder.decode(nameEncoded, "UTF-8")
    implicit val wikiContext = WikiContext(name)

    val wikiFirstRevision: DirectQuery.Page = MockDb.selectPageFirstRevision(name).getOrElse(new DirectQuery.Page("", ""))
    val wikiLastRevision: DirectQuery.Page = MockDb.selectPageLastRevision(name).getOrElse(new DirectQuery.Page("", ""))
    val wikiSpecificRevision: Option[DirectQuery.Page] = MockDb.selectPage(name, revision)

    wikiSpecificRevision match {
      case Some(page) =>
        val pageContent: PageContent = new PageContent(page.content)
        (action match {
          case "" | "view" =>
            try {
              if (WikiPermission.isReadable(pageContent)) {
                pageContent.redirect match {
                  case Some(directive) =>
                    Redirect(directive).flashing("success" -> s"""Redirected from <a href="${page.name}?action=edit">${page.name}</a>""")
                  case None =>
                    val similarPages = getSimilarPages(name)
                    val backlinks = getBacklinks(name)
                    val additionalInfo = similarPages + backlinks

                    pageContent.interpreter match {
                      case Some("Paper") =>
                        val content = Interpreters(page.content)
                        Ok(views.html.Wiki.viewOthers(name, content, wikiFirstRevision, wikiLastRevision))
                      case None | Some("Wiki") =>
                        val content = """<div class="limitWidth"><div class="wikiContent">""" + Interpreters(page.content + additionalInfo) + """</div></div>"""
                        Ok(views.html.Wiki.viewOthers(name, content, wikiFirstRevision, wikiLastRevision))
                      case _ =>
                        val content = s"""<div class="limitWidth"><div class="wikiContent"><h1>$name</h1>""" + Interpreters(page.content) + Interpreters(additionalInfo) + """</div></div>"""
                        Ok(views.html.Wiki.viewOthers(name, content, wikiFirstRevision, wikiLastRevision))
                    }
                }
              } else {
                Forbidden(views.html.Wiki.permissionDenied(name))
              }
            }
            finally {
              if (play.Play.isDev)
                actorSimilarPage ! Calculate(name)
            }
          case "raw" =>
            if (WikiPermission.isReadable(pageContent)) {
              Ok(page.content)
            } else {
              Forbidden(s"= $name\nPermission denied\n\n")
            }

          case "edit" =>
            if (WikiPermission.isWritable(pageContent)) {
              Ok(views.html.Wiki.edit(page))
            } else {
              Ok(views.html.Wiki.permissionDenied(name))
            }
          case "history" =>
            if (WikiPermission.isReadable(pageContent)) {
              Ok(views.html.Wiki.history(name, DirectQuery.pageSelectHistory(name)))
            } else {
              Ok(views.html.Wiki.permissionDenied(name))
            }
        }).withCookies(Cookie(RecentlyVisited.cookieKey, RecentlyVisited.valueStr(request, name)))
      case None =>
        action match {
          case "edit" =>
            if (WikiPermission.isWritable(new PageContent(""))) {
              Ok(views.html.Wiki.edit(new models.DirectQuery.Page(name, s"""= $name\ndescribe $name here.""")))
            } else {
              Ok(views.html.Wiki.permissionDenied(name))
            }
          case _ =>
            NotFound(views.html.Wiki.notFound(name))
        }
    }
  }


  def getSimilarPages(name: String): String = {
    val similarPages = DirectQuery.cosineSimilaritySelect(name)
      .map(c => s" * [[LinkWithPercent(${c.name2},${"%2.2f".format(c.similarity * 100)}%)]]")
      .mkString("\n")

    if (similarPages != "") {
      s"""
         |== Similar Pages
         |$similarPages
         |""".stripMargin
    }
    else {
      ""
    }
  }


  def getBacklinks(name: String): String = {
    val relationship: List[Link] = DirectQuery.linkSelect(name)
    val back: List[Link] = relationship.flatMap(lm => DirectQuery.linkSelect(lm.src))
    val forward: List[Link] = relationship.flatMap(lm => DirectQuery.linkSelect(lm.dst))

    val result = (relationship ++ back ++ forward)
      .map(l => s"${l.src}->${l.dst}")
      .mkString("\n")

    if (result != "") {
      s"""
         |== Related Pages
         |{{{#!Graph
         |#!enableWikiLink
         |$result
         |}}}
         |""".stripMargin
    }
    else {
      ""
    }
  }

  val actorSimilarPage = system.actorOf(ActorPageProcessor.props)

  def save(nameEncoded: String) = PostAction { implicit request =>
    val name = URLDecoder.decode(nameEncoded, "UTF-8")
    val (revision, body, comment) = Form(tuple("revision" -> number, "text" -> text, "comment" -> text)).bindFromRequest.get
    val (latestText, latestRevision) = MockDb.selectPageLastRevision(name).map(w => (w.content, w.revision)).getOrElse(("", 0))

    if (WikiPermission.isWritable(new PageContent(latestText))) {
      if (revision == latestRevision) {
        DirectQuery.pageInsert(name, revision + 1, DateTimeUtil.nowEpochNano, SessionLogic.getId(request).getOrElse("anonymous"), request.remoteAddress, body, comment)
        actorSimilarPage ! Calculate(name)
        Ok("")
      } else {
        Conflict("")
      }
    } else {
      Forbidden("")
    }
  }

  def preview() = PostAction { implicit request =>
    val (name, body) = Form(tuple("name" -> text, "text" -> text)).bindFromRequest.get
    implicit val wikiContext = WikiContext(name)
    Ok("""<div class="limitWidth"><div class="wikiContent preview">""" + Interpreters(body) + """</div></div>""")
  }

}





