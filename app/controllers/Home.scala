package controllers

import java.net.URLEncoder

import com.aha00a.commons.Implicits._
import javax.inject._
import logics.{AhaWikiCache, ApplicationConf}
import models.{AhaWikiQuery, PageContent}
import play.api.Configuration
import play.api.cache.CacheApi
import play.api.mvc._

class Home @Inject() (
                       implicit
                       cacheApi: CacheApi,
                       database:play.api.db.Database,
                       configuration: Configuration
                     ) extends Controller {
  def index: Action[AnyContent] = Action { implicit request =>
    Redirect(routes.Wiki.view("FrontPage", 0, "")).flashing(request.flash)
  }

  def random: Action[AnyContent] = Action { implicit request =>
    Redirect(routes.Wiki.view(AhaWikiCache.PageNameSet.get.toSeq.random(), 0, "")).flashing(request.flash)
  }

  def robotsTxt: Action[AnyContent] = Action { implicit request =>
    database.withConnection { implicit connection =>
      Ok(AhaWikiQuery().Page.selectLastRevision(".robots.txt").map(p => PageContent(p.content).content).getOrElse(""))
    }
  }

  def adsTxt: Action[AnyContent] = Action { implicit request =>
    ApplicationConf().AhaWiki.google.AdSense.adsTxtContent().toOption
      .map(s => Ok(s))
      .getOrElse(NotFound(""))
  }
}
