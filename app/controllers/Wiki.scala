package controllers

import java.net.URLDecoder
import javax.inject.{Singleton, _}

import actionCompositions.PostAction
import actors.ActorPageProcessor
import actors.ActorPageProcessor.Calculate
import akka.actor._
import logics._
import logics.wikis.{Interpreters, WikiPermission}
import models.DirectQuery.Link
import models.{DirectQuery, MockDb, PageContent, WikiContext}
import play.api.cache.CacheApi
import play.api.data.Form
import play.api.data.Forms._
import play.api.mvc._
import utils._

@Singleton
class Wiki @Inject()(implicit cacheApi: CacheApi, actorSystem: ActorSystem) extends Controller {

  def view(nameEncoded: String, revision: Int, action: String) = Action { implicit request =>
    val name = URLDecoder.decode(nameEncoded, "UTF-8")
    implicit val wikiContext = WikiContext(name)

    val wikiFirstRevision: DirectQuery.Page = MockDb.selectPageFirstRevision(name).getOrElse(new DirectQuery.Page("", ""))
    val wikiLastRevision: DirectQuery.Page = MockDb.selectPageLastRevision(name).getOrElse(new DirectQuery.Page("", ""))
    val wikiSpecificRevision: Option[DirectQuery.Page] = MockDb.selectPage(name, revision, wikiLastRevision)

    wikiSpecificRevision match {
      case Some(page) =>
        val pageContent: PageContent = new PageContent(page.content)
        action match {
          case "" | "view" =>
            try {
              if (WikiPermission.isReadable(pageContent)) {
                pageContent.redirect match {
                  case Some(directive) =>
                    Redirect(directive).flashing("success" -> s"""Redirected from <a href="${page.name}?action=edit">${page.name}</a>""")
                  case None =>
                    val similarPages = getSimilarPages(name)
                    val relatedPages = getRelatedPages(name)
                    val additionalInfo =
                      s"""
                         |== See also
                         |[[Html(<table class="seeAlso"><tr><th>Similar Pages</th><th>Related Pages</th></tr><tr><td class="">)]]
                         |$similarPages
                         |[[Html(</td><td class="">)]]$relatedPages[[Html(</td></tr></table>)]]
                         |""".stripMargin

                    pageContent.interpreter match {
                      case Some("Paper") =>
                        val content = Interpreters.interpret(page.content)
                        Ok(views.html.Wiki.viewOthers(name, content, wikiFirstRevision, wikiLastRevision))
                      case None | Some("Wiki") =>
                        val content = """<div class="limitWidth"><div class="wikiContent">""" + Interpreters.interpret(page.content + additionalInfo) + """</div></div>"""
                        Ok(views.html.Wiki.viewOthers(name, content, wikiFirstRevision, wikiLastRevision))
                      case _ =>
                        val content = s"""<div class="limitWidth"><div class="wikiContent"><h1>$name</h1>""" + Interpreters.interpret(page.content) + Interpreters.interpret(additionalInfo) + """</div></div>"""
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
        }
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
      similarPages
    }
    else {
      ""
    }
  }


  def getRelatedPages(name: String): String = {
    val relationship: List[Link] = DirectQuery.linkSelect(name)
    val back: List[Link] = relationship.flatMap(lm => DirectQuery.linkSelect(lm.src))
    val forward: List[Link] = relationship.flatMap(lm => DirectQuery.linkSelect(lm.dst))

    val result = (relationship ++ back ++ forward)
      .map(l => s"${l.src}->${l.dst}")
      .mkString("\n")

    if (result != "") {
      s"""{{{#!Graph
         |#!enableWikiLink
         |$result
         |}}}""".stripMargin
    }
    else {
      ""
    }
  }

  val actorSimilarPage = actorSystem.actorOf(ActorPageProcessor.props)

  def save(nameEncoded: String) = PostAction { implicit request =>
    val name = URLDecoder.decode(nameEncoded, "UTF-8")
    implicit val wikiContext = WikiContext(name)

    val (revision, body, comment) = Form(tuple("revision" -> number, "text" -> text, "comment" -> text)).bindFromRequest.get
    val (latestText, latestRevision) = MockDb.selectPageLastRevision(name).map(w => (w.content, w.revision)).getOrElse(("", 0))

    if (WikiPermission.isWritable(new PageContent(latestText))) {
      if (revision == latestRevision) {
        DirectQuery.pageInsert(name, revision + 1, DateTimeUtil.nowEpochNano, SessionLogic.getId(request).getOrElse("anonymous"), request.remoteAddress, body, comment)
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

  def preview() = PostAction { implicit request =>
    val (name, body) = Form(tuple("name" -> text, "text" -> text)).bindFromRequest.get
    implicit val wikiContext = WikiContext(name)
    Ok("""<div class="limitWidth"><div class="wikiContent preview">""" + Interpreters.interpret(body) + """</div></div>""")
  }

}





