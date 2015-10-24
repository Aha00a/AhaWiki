package controllers

import java.net.URLDecoder
import javax.inject.{Singleton, _}

import actionCompositions.PostAction
import actors.ActorPageProcessor
import actors.ActorPageProcessor.Calculate
import akka.actor._
import logics._
import logics.wikis.{Interpreters, WikiPermission}
import models.Database.Link
import models.{Database, MockDb, PageContent, WikiContext}
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

    val pageFirstRevision: Database.Page = MockDb.selectPageFirstRevision(name).getOrElse(new Database.Page("", ""))
    val pageLastRevision: Database.Page = MockDb.selectPageLastRevision(name).getOrElse(new Database.Page("", ""))
    val pageSpecificRevision: Option[Database.Page] = MockDb.selectPage(name, revision)

    val pageLastRevisionContent: PageContent = new PageContent(pageLastRevision.content)
    val isWritable: Boolean = WikiPermission.isWritable(pageLastRevisionContent)
    val isReadable: Boolean = WikiPermission.isReadable(pageLastRevisionContent)

    pageSpecificRevision match {
      case Some(page) =>
        val pageContent: PageContent = new PageContent(page.content)
        action match {
          case "" | "view" => {
            try {
              if (isReadable) {
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

                    Ok(pageContent.interpreter match {
                      case Some("Paper") =>
                        val contentInterpreted = Interpreters.interpret(page.content)
                        views.html.Wiki.view(name, contentInterpreted, isWritable, pageFirstRevision, pageLastRevision)
                      case None | Some("Wiki") =>
                        val contentInterpreted = """<div class="limitWidth"><div class="wikiContent">""" + Interpreters.interpret(page.content + additionalInfo) + """</div></div>"""
                        views.html.Wiki.view(name, contentInterpreted, isWritable, pageFirstRevision, pageLastRevision)
                      case _ =>
                        val contentInterpreted = s"""<div class="limitWidth"><div class="wikiContent"><h1>$name</h1>""" + Interpreters.interpret(page.content) + Interpreters.interpret(additionalInfo) + """</div></div>"""
                        views.html.Wiki.view(name, contentInterpreted, isWritable, pageFirstRevision, pageLastRevision)
                    })
                }
              } else {
                Forbidden(views.html.Wiki.permissionDenied(name))
              }
            }
            finally {
              if (play.Play.isDev)
                actorSimilarPage ! Calculate(name)
            }
          }
          case "raw" => {
            if (isReadable) {
              Ok(page.content)
            } else {
              Forbidden(s"= $name\nPermission denied\n\n")
            }
          }
          case "edit" => {
            if (isWritable) {
              Ok(views.html.Wiki.edit(page))
            } else {
              Ok(views.html.Wiki.permissionDenied(name))
            }
          }
          case "history" => {
            if (isReadable) {
              Ok(views.html.Wiki.history(name, Database.pageSelectHistory(name)))
            } else {
              Ok(views.html.Wiki.permissionDenied(name))
            }
          }
        }
      case None =>
        action match {
          case "edit" => {
            if (WikiPermission.isWritable(new PageContent(""))) {
              Ok(views.html.Wiki.edit(new models.Database.Page(name, s"""= $name\ndescribe $name here.""")))
            } else {
              Ok(views.html.Wiki.permissionDenied(name))
            }
          }
          case _ => {
            val relatedPages = getRelatedPages(name)
            val additionalInfo =
              s"""= $name
                 |This page does not exist. You can [?action=edit create] it here.
                 |= See also
                 |[[Html(<table class="seeAlso"><tr><th>Similar Pages</th><th>Related Pages</th></tr><tr><td class="">)]]
                 |[[Html(</td><td class="">)]]$relatedPages[[Html(</td></tr></table>)]]
                 |""".stripMargin

            NotFound(views.html.Wiki.notFound(name, Interpreters.interpret(additionalInfo)))
          }
        }
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

  val actorSimilarPage = actorSystem.actorOf(ActorPageProcessor.props)

  def save(nameEncoded: String) = PostAction { implicit request =>
    val name = URLDecoder.decode(nameEncoded, "UTF-8")
    implicit val wikiContext = WikiContext(name)

    val (revision, body, comment) = Form(tuple("revision" -> number, "text" -> text, "comment" -> text)).bindFromRequest.get
    val (latestText, latestRevision) = MockDb.selectPageLastRevision(name).map(w => (w.content, w.revision)).getOrElse(("", 0))

    if (WikiPermission.isWritable(new PageContent(latestText))) {
      if (revision == latestRevision) {
        Database.pageInsert(name, revision + 1, DateTimeUtil.nowEpochNano, SessionLogic.getId(request).getOrElse("anonymous"), request.remoteAddress, body, comment)
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





