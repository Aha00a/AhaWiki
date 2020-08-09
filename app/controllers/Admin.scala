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
                     syncCacheApi: SyncCacheApi,
                     actorSystem: ActorSystem,
                     database: Database,
                     environment: Environment,
                     @Named("db-actor") actorAhaWiki: ActorRef,
                     configuration: Configuration,
                     wsClient: WSClient,
                     executionContext: ExecutionContext
                   ) extends BaseController {
  def index(): Action[AnyContent] = Action { implicit request =>
    import com.aha00a.play.PlayContext
    
    implicit val playContext: PlayContext = PlayContext()
    Ok(views.html.Admin.index())
  }
}

