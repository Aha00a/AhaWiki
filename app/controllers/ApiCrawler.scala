package controllers

import org.apache.pekko.actor.ActorRef
import org.apache.pekko.actor.ActorSystem
import com.aha00a.play.Implicits.RichRequest
import io.circe.Json
import io.circe.generic.auto._
import logics.{AhaWikiCache, AhaWikiCacheMemoryApiLinks, ApplicationConf, Crawler, CrawlerUrlNormalizer, CrawlerUrlSafety}
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
import io.circe.syntax._

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
  private def isAdmin(implicit request: RequestHeader): Boolean =
    logics.AdminLogic.isAdmin(request)

  def get(q: String): Action[AnyContent] = Action { implicit request =>
    try {
      logger.info(s"${request.remoteAddressWithXRealIp}\t$q")
      val normalizedUrl = CrawlerUrlNormalizer.normalize(q)
      if (normalizedUrl.length > CacheCrawler.UrlMaxLength) {
        Forbidden(Json.obj(
          "message" -> Json.fromString(s"URL too long: max ${CacheCrawler.UrlMaxLength} characters")
        ))
      } else CrawlerUrlSafety.validate(normalizedUrl) match {
        case Left(message) => Forbidden(Json.obj("message" -> Json.fromString(message)))
        case Right(_) => database.withConnection { implicit connection =>
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

  def adminList(
    page: Int,
    pageSize: Int,
    search: String,
    sortBy: String,
    sortOrder: String,
  ): Action[AnyContent] = Action { implicit request =>
    if (!isAdmin) Forbidden("Access denied.")
    else {
      val safePage = math.max(1, page)
      val safePageSize = math.max(1, math.min(pageSize, 100))
      database.withConnection { implicit connection =>
        Ok(Json.obj(
          "array" -> CacheCrawler.selectPaged(
            page = safePage,
            pageSize = safePageSize,
            search = search,
            sortBy = sortBy,
            sortOrder = sortOrder,
          ).asJson,
          "count" -> Json.fromLong(CacheCrawler.countPaged(search)),
        ))
      }
    }
  }

  def adminDelete(url: String): Action[AnyContent] = Action { implicit request =>
    if (!isAdmin) Forbidden("Access denied.")
    else {
      val normalizedUrl = CrawlerUrlNormalizer.normalize(url)
      database.withConnection { implicit connection =>
        Ok(Json.obj("deleted" -> Json.fromInt(CacheCrawler.deleteByUrl(normalizedUrl))))
      }
    }
  }

  def adminRefresh(url: String): Action[AnyContent] = Action { implicit request =>
    if (!isAdmin) Forbidden("Access denied.")
    else {
      val normalizedUrl = CrawlerUrlNormalizer.normalize(url)
      CrawlerUrlSafety.validate(normalizedUrl) match {
        case Left(message) => Forbidden(Json.obj("message" -> Json.fromString(message)))
        case Right(_) =>
          val crawler = Crawler.fromUrl(normalizedUrl)(logger)
          database.withConnection { implicit connection =>
            CacheCrawler.upsertDone(normalizedUrl, crawler.title, crawler.image, crawler.description)
          }
          Ok(Json.obj("success" -> Json.fromBoolean(true), "url" -> Json.fromString(normalizedUrl)))
      }
    }
  }
}
