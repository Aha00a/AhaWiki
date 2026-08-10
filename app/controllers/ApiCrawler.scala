package controllers

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
import scala.concurrent.{ExecutionContext, Future}
import io.circe.syntax._

//noinspection TypeAnnotation
class ApiCrawler @Inject()(
  implicit val
  controllerComponents: ControllerComponents,
  actorSystem: ActorSystem,
  database: Database,
  applicationConf: ApplicationConf,
  ahaWikiCache: AhaWikiCache,
  wsClient: WSClient,
  executionContext: ExecutionContext,
  applicationLifecycleHook: ApplicationLifecycleHook,
  configuration: Configuration,
  syncCacheApi: SyncCacheApi,
  ahaWikiCacheMemoryApiLinks: AhaWikiCacheMemoryApiLinks,
) extends BaseController with JsonResults with AdminAuth with Logging {

  private val crawlerExecutionContext: ExecutionContext =
    actorSystem.dispatchers.lookup("ahawiki-blocking-io-dispatcher")

  def get(q: String): Action[AnyContent] = Action.async { implicit request =>
    try {
      logger.info(s"${request.remoteAddressWithXRealIp}\t$q")
      val normalizedUrl = CrawlerUrlNormalizer.normalize(q)
      if (normalizedUrl.length > CacheCrawler.UrlMaxLength) {
        Future.successful(JsonResult(Forbidden, Json.obj(
          "message" -> Json.fromString(s"URL too long: max ${CacheCrawler.UrlMaxLength} characters")
        )))
      } else CrawlerUrlSafety.validate(normalizedUrl) match {
        case Left(message) => Future.successful(JsonResult(Forbidden, Json.obj("message" -> Json.fromString(message))))
        case Right(_) =>
          val cached = database.withConnection { implicit connection =>
            CacheCrawler.selectByUrl(normalizedUrl)
          }

          cached match {
            case Some(cache) if CacheCrawler.isFresh(cache) =>
              logger.info(s"Cache Hit\tFresh\t$normalizedUrl")
              Future.successful(Ok(Json.obj(
                "success"     -> Json.fromBoolean(true),
                "cache"       -> Json.fromString("hit"),
                "title"       -> Json.fromString(cache.title),
                "image"       -> Json.fromString(cache.image),
                "description" -> Json.fromString(cache.description),
              )))

            case Some(cache) if CacheCrawler.isStaleButRevalidatable(cache) =>
              logger.info(s"Cache Hit\tStale\t$normalizedUrl")
              Future {
                val crawler = Crawler.fromUrl(normalizedUrl)(logger)
                database.withConnection { implicit connection =>
                  CacheCrawler.upsertDone(normalizedUrl, crawler.title, crawler.image, crawler.description)
                }
              }(crawlerExecutionContext).recover {
                case e: Exception =>
                  logger.warn(s"CacheCrawler refresh failed: $normalizedUrl", e)
              }(executionContext)

              Future.successful(Ok(Json.obj(
                "success"     -> Json.fromBoolean(true),
                "cache"       -> Json.fromString("stale"),
                "title"       -> Json.fromString(cache.title),
                "image"       -> Json.fromString(cache.image),
                "description" -> Json.fromString(cache.description),
              )))

            case _ =>
              logger.info(s"Cache Miss\t$normalizedUrl")
              Future {
                val crawler = Crawler.fromUrl(normalizedUrl)(logger)
                database.withConnection { implicit connection =>
                  CacheCrawler.upsertDone(normalizedUrl, crawler.title, crawler.image, crawler.description)
                }
                crawler
              }(crawlerExecutionContext).map { crawler =>
                Ok(Json.obj(
                  "success"     -> Json.fromBoolean(true),
                  "cache"       -> Json.fromString("miss"),
                  "title"       -> Json.fromString(crawler.title),
                  "image"       -> Json.fromString(crawler.image),
                  "description" -> Json.fromString(crawler.description),
                ))
              }(executionContext).recover {
                case e: Exception =>
                  JsonResult(Forbidden, Json.obj("message" -> Json.fromString(e.getMessage)))
              }(executionContext)
          }
      }
    }
    catch {
      case e: Exception =>
        Future.successful(JsonResult(Forbidden, Json.obj(
          "message" -> Json.fromString(e.getMessage)
        )))
    }
  }

  def adminList(
    page: Int,
    pageSize: Int,
    search: String,
    sortBy: String,
    sortOrder: String,
  ): Action[AnyContent] = Action { implicit request =>
    if (!isAdmin) AccessDenied
    else {
      val safePage = math.max(1, page)
      val safePageSize = math.max(1, math.min(pageSize, 100))
      database.withConnection { implicit connection =>
        Ok(pagedJson(
          CacheCrawler.selectPaged(
            page = safePage,
            pageSize = safePageSize,
            search = search,
            sortBy = sortBy,
            sortOrder = sortOrder,
          ).asJson,
          safePage,
          safePageSize,
          CacheCrawler.countPaged(search),
        ))
      }
    }
  }

  def adminDelete(url: String): Action[AnyContent] = Action { implicit request =>
    if (!isAdmin) AccessDenied
    else {
      val normalizedUrl = CrawlerUrlNormalizer.normalize(url)
      database.withConnection { implicit connection =>
        Ok(Json.obj("deleted" -> Json.fromInt(CacheCrawler.deleteByUrl(normalizedUrl))))
      }
    }
  }

  def adminRefresh(url: String): Action[AnyContent] = Action.async { implicit request =>
    if (!isAdmin) Future.successful(AccessDenied)
    else {
      try {
        val normalizedUrl = CrawlerUrlNormalizer.normalize(url)
        CrawlerUrlSafety.validate(normalizedUrl) match {
          case Left(message) => Future.successful(JsonResult(Forbidden, Json.obj("message" -> Json.fromString(message))))
          case Right(_) =>
            Future {
              val crawler = Crawler.fromUrl(normalizedUrl)(logger)
              database.withConnection { implicit connection =>
                CacheCrawler.upsertDone(normalizedUrl, crawler.title, crawler.image, crawler.description)
              }
            }(crawlerExecutionContext).map { _ =>
              Ok(Json.obj("success" -> Json.fromBoolean(true), "url" -> Json.fromString(normalizedUrl)))
            }(executionContext).recover {
              case e: Exception =>
                JsonResult(Forbidden, Json.obj("message" -> Json.fromString(e.getMessage)))
            }(executionContext)
        }
      } catch {
        case e: Exception =>
          Future.successful(JsonResult(Forbidden, Json.obj("message" -> Json.fromString(e.getMessage))))
      }
    }
  }
}
