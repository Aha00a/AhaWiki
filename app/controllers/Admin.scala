package controllers

import akka.actor.ActorRef
import akka.actor.ActorSystem
import logics.ApplicationConf
import logics.{AhaWikiCache, SessionLogic}
import models.ContextSite
import models.tables.{Site, UserSite}
import play.api.db.Database
import play.api.libs.ws.WSClient
import play.api.mvc._

import javax.inject.Inject
import javax.inject.Named
import scala.concurrent.ExecutionContext

class Admin @Inject()(
  implicit val
  controllerComponents: ControllerComponents,
  actorSystem: ActorSystem,
  database: Database,
  @Named("db-actor") actorAhaWiki: ActorRef,
  ahaWikiCache: AhaWikiCache,
  wsClient: WSClient,
  applicationConf: ApplicationConf,
  executionContext: ExecutionContext
) extends BaseController {

  private def isAdmin(implicit request: RequestHeader): Boolean = {
    SessionLogic.getUser(request).exists(u => u.email == "aha00a@gmail.com" || u.seq == 1)
  }

  def index(): Action[AnyContent] = Action { implicit request =>
    if (isAdmin) {
      Ok(views.html.Admin.index())
    } else {
      Forbidden("Access denied.")
    }
  }

  def site(seq: Long): Action[AnyContent] = Action { implicit request =>
    if (isAdmin) {
      Ok(views.html.Admin.index())
    } else {
      Forbidden("Access denied.")
    }
  }

  def sites(): Action[AnyContent] = Action { implicit request =>
    if (isAdmin) {
      database.withConnection { implicit connection =>
        val seqSite = Site.select()
        Ok(views.html.Admin.sites(seqSite))
      }
    } else {
      Forbidden("Access denied.")
    }
  }

  def siteUsers(): Action[AnyContent] = Action { implicit request =>
    if (isAdmin) {
      database.withConnection { implicit connection =>
        implicit val site: Site = logics.SiteLogic.get(request.host)
        val seqUserSite = UserSite.select()
        Ok(views.html.Admin.siteUsers(seqUserSite)(ContextSite()))
      }
    } else {
      Forbidden("Access denied.")
    }
  }
}
