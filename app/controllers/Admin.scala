package controllers

import akka.actor.ActorRef
import akka.actor.ActorSystem
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
  wsClient: WSClient,
  executionContext: ExecutionContext
) extends BaseController {
  def index(): Action[AnyContent] = Action { implicit request =>
    Ok(views.html.Admin.index())
  }
}

