package controllers

import akka.actor.ActorRef
import akka.actor.ActorSystem
import com.aha00a.play.Implicits.RichRequest
import io.circe.Json
import logics.{AhaWikiCache, AhaWikiCacheMemoryApiLinks, ApplicationConf, Crawler, CrawlerUrlNormalizer}
import models.tables.CacheCrawler
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

  def get(q: String): Action[AnyContent] = Action { implicit request =>
    try {
      logger.info(s"${request.remoteAddressWithXRealIp}\t$q")
      val normalizedUrl = CrawlerUrlNormalizer.normalize(q)
      if (normalizedUrl.length > CacheCrawler.UrlMaxLength) {
        Forbidden(Json.obj(
          "message" -> Json.fromString(s"URL too long: max ${CacheCrawler.UrlMaxLength} characters")
        ))
      } else {
        database.withConnection { implicit connection =>
          CacheCrawler.selectByUrl(normalizedUrl) match {
            case Some(cache) if CacheCrawler.isFresh(cache) =>
              logger.info(s"Cache Hit\tFresh\t${normalizedUrl}")
              Ok(Json.obj(
                "success" -> Json.fromBoolean(true),
                "cache" -> Json.fromString("hit"),
                "title" -> Json.fromString(cache.title),
                "image" -> Json.fromString(cache.image),
                "description" -> Json.fromString(cache.description),
              ))

            case Some(cache) if CacheCrawler.isStaleButRevalidatable(cache) =>
              logger.info(s"Cache Hit\tStale\t${normalizedUrl}")
              actorSystem.dispatcher.execute(() => database.withConnection { implicit connection2 =>
                val crawler = Crawler.fromUrl(normalizedUrl)(logger)
                CacheCrawler.upsertDone(normalizedUrl, crawler.title, crawler.image, crawler.description)(connection2)
              })
              Ok(Json.obj(
                "success" -> Json.fromBoolean(true),
                "cache" -> Json.fromString("stale"),
                "title" -> Json.fromString(cache.title),
                "image" -> Json.fromString(cache.image),
                "description" -> Json.fromString(cache.description),
              ))

            case _ =>
              logger.info(s"Cache Miss\t${normalizedUrl}")
              val crawler = Crawler.fromUrl(normalizedUrl)(logger)
              CacheCrawler.upsertDone(normalizedUrl, crawler.title, crawler.image, crawler.description)
              Ok(Json.obj(
                "success" -> Json.fromBoolean(true),
                "cache" -> Json.fromString("miss"),
                "title" -> Json.fromString(crawler.title),
                "image" -> Json.fromString(crawler.image),
                "description" -> Json.fromString(crawler.description),
              ))
          }
        }
      }
    }
    catch {
      case e: Exception =>
        Forbidden(Json.obj(
          "message" -> Json.fromString(e.getMessage)
        ))
    }
  }
}
