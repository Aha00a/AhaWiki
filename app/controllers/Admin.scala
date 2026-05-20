package controllers

import org.apache.pekko.actor.ActorRef
import org.apache.pekko.actor.ActorSystem
import logics.ApplicationConf
import logics.{AhaWikiCache, SessionLogic}
import models.tables.Site
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

  private def isAdmin(implicit request: RequestHeader): Boolean =
    logics.AdminLogic.isAdmin(request)

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
}
