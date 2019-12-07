package controllers

import java.net.URLEncoder

import com.aha00a.commons.implicits.Implicits
import com.aha00a.commons.implicits.Implicits._
import javax.inject._
import logics.AhaWikiCache
import models.{AhaWikiDatabase, PageContent, WikiContext}
import play.api.cache.CacheApi
import play.api.mvc._

class Home @Inject() (implicit cacheApi: CacheApi, database:play.api.db.Database) extends Controller {
  def index = Action { implicit request =>
    Redirect(routes.Wiki.view("FrontPage", 0, "")).flashing(request.flash)
  }

  def random = Action { implicit request =>
    implicit val wikiContext: WikiContext = WikiContext("")

    Redirect(routes.Wiki.view(URLEncoder.encode(AhaWikiCache.PageNameSet.get.toSeq.random(), "UTF-8"), 0, "")).flashing(request.flash)
  }

  def robotsTxt = Action { implicit request =>
    Ok(AhaWikiDatabase().pageSelectLastRevision(".robots.txt").map(p => PageContent(p.content).content).getOrElse(""))
  }
}
