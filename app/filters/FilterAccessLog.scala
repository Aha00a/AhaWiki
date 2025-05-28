package filters

import akka.actor.ActorRef
import akka.actor.ActorSystem
import akka.pattern.after
import akka.stream.Materializer
import com.aha00a.commons.Implicits._
import com.aha00a.play.Implicits._
import logics.AhaWikiCache
import logics.ApplicationConf
import logics.SessionLogic
import logics.SiteLogic
import models.tables.IpDeny
import models.tables.Site
import play.api.Environment
import play.api.Logging
import play.api.db.Database
import play.api.libs.ws.WSClient
import play.api.mvc._

import java.sql.Connection
import javax.inject.Inject
import javax.inject.Named
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.Random


class FilterAccessLog @Inject()(
  implicit
  val mat: Materializer,
  actorSystem: ActorSystem,
  database: Database,
  environment: Environment,
  @Named("db-actor") actorAhaWiki: ActorRef,
  applicationConf: ApplicationConf,
  ahaWikiCache: AhaWikiCache,
  wsClient: WSClient,
  executionContext: ExecutionContext
) extends Filter with Logging {
  private val toCheckStartsWith = Seq(
    "/wp",
    "/wordpress",
    "/new",
    "/old",
    "/backup",
    "/blog",
    "/config",
    "/.aws",
    "/.env",
    "/.git",
    "/.vscode",
  )

  //noinspection SpellCheckingInspection
  private val toCheckContains = Seq(
    "/wp-admin",
    "/wp-atom",
    "/wp-comments-post",
    "/wp-content",
    "/wp-cron",
    "/wp-feed",
    "/wp-feed-rss",
    "/wp-includes",
    "/wp-json",
    "/wp-login.php",
    "/wp-rss",
    "/wp-trackback",
    "/phpinfo.php",
    "/php_info.php",
    "/temp.php",
    "/xmlrpc.php",
  )

  private def isUriAttack(uri: String): Boolean = {
    toCheckStartsWith.exists(uri.startsWith) || toCheckContains.exists(uri.contains)
  }

  private def logRequest(method: String, status: Int, duration: Long, remoteAddress: String, url: String, userAgent: String): Unit = {
    logger.info(Seq(
      method.padRight(7),
      status,
      s"${duration}ms".padLeft(7),
      remoteAddress.padRight(15),
      url,
      userAgent
    ).mkString("\t"))
  }

  override def apply(nextFilter: RequestHeader => Future[Result])(requestHeader: RequestHeader): Future[Result] = {
    val startTime = System.currentTimeMillis
    val scheme = requestHeader.scheme
    val host = requestHeader.host
    val uri: String = requestHeader.uri
    val url = s"$scheme://$host$uri" // TODO: remove
    val remoteAddress = requestHeader.remoteAddressWithXRealIp
    val userAgent = requestHeader.userAgent.getOrElse("")
    val (optionIpDeny: Option[IpDeny], siteFound) = database.withConnection { implicit connection =>
      implicit val site: Site = SiteLogic.get(requestHeader.host)
      (models.tables.IpDeny.selectLatest(remoteAddress), site)
    }
    implicit val site: Site = siteFound

    if (optionIpDeny.isDefined) {
      logger.warn(s"${requestHeader.method}\t\tAttack\t$remoteAddress\t$url\t$userAgent")
      after((Random.nextInt(5 * 60) + 60).seconds, actorSystem.scheduler)({
        val endTime = System.currentTimeMillis
        val duration = endTime - startTime
        logRequest(requestHeader.method, Results.Forbidden.header.status, duration, remoteAddress, url, userAgent)
        database.withConnection { implicit connection =>
          models.tables.AccessLog.insert(
            site.seq,
            getUserSeq(requestHeader),
            optionIpDeny.map(_.seq),
            requestHeader.method,
            scheme,
            host,
            uri,
            url,
            remoteAddress,
            userAgent,
            Results.Forbidden.header.status,
            duration.toInt
          )
        }
        Future(Results.Forbidden)
      })
    } else if (isUriAttack(uri)) {
      logger.warn(s"${requestHeader.method}\t\tAttack\t$remoteAddress\t$url\t$userAgent")
      after((Random.nextInt(5 * 60) + 60).seconds, actorSystem.scheduler)({
        val endTime = System.currentTimeMillis
        val duration = endTime - startTime
        logRequest(requestHeader.method, 403, duration, remoteAddress, url, userAgent)
        database.withConnection { implicit connection =>
          val accessLogSeq = models.tables.AccessLog.insert(
            site.seq,
            getUserSeq(requestHeader),
            None,
            requestHeader.method,
            scheme,
            host,
            uri,
            url,
            remoteAddress,
            userAgent,
            Results.Forbidden.header.status,
            duration.toInt
          )
          models.tables.IpDeny.insert(remoteAddress, accessLogSeq, s"$scheme://$host$uri")
        }
        Future(Results.Forbidden)
      })
    } else {
      nextFilter(requestHeader).map(result => {
        val endTime = System.currentTimeMillis
        val duration = endTime - startTime
        logRequest(requestHeader.method, result.header.status, duration, remoteAddress, url, userAgent)
        database.withConnection { implicit connection =>
          models.tables.AccessLog.insert(
            site.seq,
            getUserSeq(requestHeader),
            None,
            requestHeader.method,
            scheme,
            host,
            uri,
            url,
            remoteAddress,
            userAgent,
            result.header.status,
            duration.toInt
          )
        }
        result.withHeaders("Request-Time" -> duration.toString)
      })
    }
  }

  private def getUserSeq(requestHeader: RequestHeader): Option[Long] = {
    SessionLogic.getUser(requestHeader).map(_.seq)
  }
}
