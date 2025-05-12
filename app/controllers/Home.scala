package controllers

import akka.actor.ActorRef
import akka.actor.ActorSystem
import com.aha00a.commons.Implicits._
import logics.AhaWikiCache
import logics.ApplicationConf
import logics.wikis.PageLogic
import models.ContextSite
import models.PageContent
import models.tables.Site
import play.api.Environment
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
                       @Named("db-actor") actorAhaWiki: ActorRef,
                       applicationConf: ApplicationConf,
                       ahaWikiCache: AhaWikiCache,
                       wsClient: WSClient,
                       executionContext: ExecutionContext
                     ) extends BaseController {
  def index: Action[AnyContent] = Action { implicit request =>
    Redirect(routes.Wiki.view("FrontPage", 0, "")).flashing(request.flash)
  }

  def random: Action[AnyContent] = Action { implicit request =>
    import com.aha00a.commons.utils.UriUtil
    import models.RequestWrapper

    implicit val provider: RequestWrapper = RequestWrapper()
    database.withConnection { implicit connection =>
      implicit val site: Site = Site.get(request.host)
      implicit val contextSite: ContextSite = ContextSite()

      val name = PageLogic.getListPageByPermission().random().name
      Redirect(routes.Wiki.view(UriUtil.encodeURIComponent(name), 0, "")).flashing(request.flash)
    }
  }

  def robotsTxt: Action[AnyContent] = Action { implicit request =>
    database.withConnection { implicit connection =>
      implicit val site: Site = Site.get(request.host)

      Ok(models.tables.Page.selectLastRevision(".robots.txt").map(p => PageContent(p.content).content).getOrElse(
        """User-agent: *
          |Disallow: /
          |
          |User-agent: Googlebot
          |Allow: /
          |
          |User-agent: Mediapartners-Google
          |Allow: /
          |
          |User-agent: DuckDuckBot
          |Allow: /
          |""".stripMargin))
    }
  }

  def adsTxt: Action[AnyContent] = Action { implicit request =>
    applicationConf.AhaWiki.google.AdSense.adsTxtContent().toOption
      .map(s => Ok(s))
      .getOrElse(NotFound(""))
  }
}
