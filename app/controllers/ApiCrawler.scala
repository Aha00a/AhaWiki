package controllers

import akka.actor.ActorRef
import akka.actor.ActorSystem
import com.aha00a.play.Implicits.RichRequest
import io.circe.Json
import logics.AhaWikiCache
import logics.AhaWikiCacheMemoryApiLinks
import logics.ApplicationConf
import play.api.Configuration
import play.api.Logging
import play.api.cache.SyncCacheApi
import play.api.db.Database
import play.api.libs.ws.WSClient
import play.api.mvc._
import services.ApplicationLifecycleHook

import javax.inject._
import scala.concurrent.ExecutionContext

//noinspection TypeAnnotation
class ApiCrawler @Inject()(
  implicit val
  controllerComponents: ControllerComponents,
  actorSystem: ActorSystem,
  database: Database,
  @Named("db-actor") actorAhaWiki: ActorRef,
  applicationConf: ApplicationConf,
  ahaWikiCache: AhaWikiCache,
  wsClient: WSClient,
  executionContext: ExecutionContext,
  applicationLifecycleHook: ApplicationLifecycleHook,
  configuration: Configuration,
  syncCacheApi: SyncCacheApi,
  ahaWikiCacheMemoryApiLinks: AhaWikiCacheMemoryApiLinks,
) extends BaseController with Logging {
  def Ok(json: io.circe.Json): Result = Ok(json.toString()).as(JSON)
  def Forbidden(json: io.circe.Json): Result = Forbidden(json.toString()).as(JSON)

  def get(q: String) = Action { implicit request =>
    try {
      logger.info(s"${request.remoteAddressWithXRealIp}\t$q")
      val crawler = logics.Crawler.fromUrl(q)
      Ok(Json.obj(
        "success" -> Json.fromBoolean(true),
        "title" -> Json.fromString(crawler.title),
        "image" -> Json.fromString(crawler.image),
        "description" -> Json.fromString(crawler.description)
      ))
    }
    catch {
      case e: Exception =>
        Forbidden(Json.obj(
          "message" -> Json.fromString(e.getMessage)
        ))
    }
  }
}
