package controllers

import java.net.URLEncoder

import actors.{ActorPageProcessor}
import actors.ActorPageProcessor.Calculate
import akka.util.Timeout
import logics.OnApplicationStart
import models.{PageContent, MockDb, DirectQuery}
import play.api.mvc._
import akka.actor._
import javax.inject._
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import utils.UuidUtil
import scala.concurrent.duration._
import akka.pattern.ask

class Home @Inject() (on:OnApplicationStart) extends Controller {
  def index = Action { implicit request =>
    Redirect(routes.Wiki.view("FrontPage", 0, "")).flashing(request.flash)
  }

  def random = Action { implicit request =>
    Redirect(routes.Wiki.view(URLEncoder.encode(DirectQuery.pageSelectNameRandom(), "UTF-8"), 0, "")).flashing(request.flash)
  }

  def robotsTxt = Action { implicit request =>
    Ok(MockDb.selectPageLastRevision(".robots.txt").map(p => new PageContent(p.content).content).getOrElse(""))
  }
}
