package controllers

import akka.actor.ActorRef
import akka.actor.ActorSystem
import javax.inject.Inject
import javax.inject.Named
import play.api.Configuration
import play.api.Environment
import play.api.cache.SyncCacheApi
import play.api.db.Database
import play.api.libs.ws.WSClient
import play.api.mvc._

import scala.concurrent.ExecutionContext

class Admin @Inject()(
  implicit val
  controllerComponents: ControllerComponents,
  actorSystem: ActorSystem,
  database: Database,
  @Named("db-actor") actorAhaWiki: ActorRef,
  configuration: Configuration,
  wsClient: WSClient,
  executionContext: ExecutionContext
) extends BaseController {
  def index(): Action[AnyContent] = Action { implicit request =>
    Ok(views.html.Admin.index())
  }
}

