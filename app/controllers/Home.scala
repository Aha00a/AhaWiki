package controllers

import java.net.URLEncoder
import javax.inject._

import implicits.Implicits._
import logics.{Cache, OnApplicationStart}
import models.{MockDb, PageContent, WikiContext}
import play.api.cache.CacheApi
import play.api.mvc._

class Home @Inject() (implicit on:OnApplicationStart, cacheApi: CacheApi) extends Controller {
  def index = Action { implicit request =>
    Redirect(routes.Wiki.view("FrontPage", 0, "")).flashing(request.flash)
  }

  def random = Action { implicit request =>
    implicit val wikiContext: WikiContext = WikiContext("")

    Redirect(routes.Wiki.view(URLEncoder.encode(Cache.PageNameSet.get.toSeq.random(), "UTF-8"), 0, "")).flashing(request.flash)
  }

  def robotsTxt = Action { implicit request =>
    Ok(MockDb.selectPageLastRevision(".robots.txt").map(p => new PageContent(p.content).content).getOrElse(""))
  }
}
