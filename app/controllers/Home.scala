package controllers

import java.net.URLEncoder

import com.aha00a.commons.Implicits._
import javax.inject._
import logics.AhaWikiCache
import models.{AhaWikiDatabase, PageContent}
import play.api.cache.CacheApi
import play.api.mvc._

class Home @Inject() (implicit cacheApi: CacheApi, database:play.api.db.Database) extends Controller {
  def index: Action[AnyContent] = Action { implicit request =>
    Redirect(routes.Wiki.view("FrontPage", 0, "")).flashing(request.flash)
  }

  def random: Action[AnyContent] = Action { implicit request =>
    Redirect(routes.Wiki.view(URLEncoder.encode(AhaWikiCache.PageNameSet.get.toSeq.random(), "UTF-8"), 0, "")).flashing(request.flash)
  }

  def robotsTxt: Action[AnyContent] = Action { implicit request =>
    Ok(AhaWikiDatabase().pageSelectLastRevision(".robots.txt").map(p => PageContent(p.content).content).getOrElse(""))
  }
}
