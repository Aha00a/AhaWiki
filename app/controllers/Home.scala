package controllers

import org.apache.pekko.actor.ActorSystem
import com.aha00a.commons.Implicits._
import com.aha00a.commons.utils.UriUtil
import logics.AhaWikiCache
import logics.ApplicationConf
import logics.SiteLogic
import models.ContextSite
import models.PageContent
import models.WikiActors
import models.tables.Site
import play.api.Environment

import java.io.File
import play.api.db.Database
import play.api.libs.ws.WSClient
import play.api.mvc._

import javax.inject._
import scala.concurrent.ExecutionContext

class Home @Inject() (
                       implicit val
                       controllerComponents: ControllerComponents,
                       actorSystem: ActorSystem,
                       database: Database,
                       environment: Environment,
                        wikiActors: WikiActors,
                       applicationConf: ApplicationConf,
                       ahaWikiCache: AhaWikiCache,
                       wsClient: WSClient,
                       executionContext: ExecutionContext
                      ) extends BaseController {
  def index: Action[AnyContent] = Action { implicit request =>
    Redirect(routes.Wiki.view("FrontPage", 0, "")).flashing(request.flash)
  }

  def random: Action[AnyContent] = Action { implicit request =>
    implicit val site: Site = SiteLogic.get(request.host)
    val contextSite: ContextSite = ContextSite()
    val name = contextSite.seqPageByPermission.random().name
    Redirect(routes.Wiki.view(UriUtil.encodeURIComponent(name), 0, "")).flashing(request.flash)
  }

  def robotsTxt: Action[AnyContent] = Action { implicit request =>
    database.withConnection { implicit connection =>
      implicit val site: Site = SiteLogic.get(request.host)

      Ok(models.tables.Page.selectLastRevision(".robots.txt").map(p => PageContent(p.content).content).getOrElse(
        PageContent(new File("app/assets/Page", ".robots.txt").readAllString()).content
      ))
    }
  }

  def adsTxt: Action[AnyContent] = Action { implicit request =>
    applicationConf.AhaWiki.google.AdSense.adsTxtContent().toOption
      .map(s => Ok(s))
      .getOrElse(NotFound(""))
  }
}
