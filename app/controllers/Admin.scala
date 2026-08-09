package controllers

import org.apache.pekko.actor.ActorSystem
import logics.ApplicationConf
import logics.{AhaWikiCache, SessionLogic}
import models.tables.Site
import play.api.db.Database
import play.api.libs.ws.WSClient
import play.api.mvc._

import javax.inject.Inject
import scala.concurrent.ExecutionContext

class Admin @Inject()(
  implicit val
  controllerComponents: ControllerComponents,
  actorSystem: ActorSystem,
  database: Database,
  ahaWikiCache: AhaWikiCache,
  wsClient: WSClient,
  applicationConf: ApplicationConf,
  executionContext: ExecutionContext
) extends BaseController with AdminAuth {

  private def currentSiteSeq(implicit request: RequestHeader): Long =
    logics.SiteLogic.get(request.host).seq

  def index(): Action[AnyContent] = Action { implicit request =>
    if (isAdmin || isSiteAdmin(currentSiteSeq)) {
      Ok(views.html.Admin.index())
    } else {
      AccessDenied
    }
  }

  def site(seq: Long): Action[AnyContent] = Action { implicit request =>
    if (isAdmin || isSiteAdmin(seq)) {
      Ok(views.html.Admin.index())
    } else {
      AccessDenied
    }
  }

  def siteSpa(seq: Long, path: String): Action[AnyContent] = Action { implicit request =>
    if (isAdmin || isSiteAdmin(seq)) {
      Ok(views.html.Admin.index())
    } else {
      AccessDenied
    }
  }

  def spa(path: String): Action[AnyContent] = Action { implicit request =>
    if (isAdmin || isSiteAdmin(currentSiteSeq)) {
      Ok(views.html.Admin.index())
    } else {
      AccessDenied
    }
  }

  def sites(): Action[AnyContent] = Action { implicit request =>
    if (isAdmin) {
      database.withConnection { implicit connection =>
        val seqSite = Site.select()
        Ok(views.html.Admin.sites(seqSite))
      }
    } else {
      AccessDenied
    }
  }
}
