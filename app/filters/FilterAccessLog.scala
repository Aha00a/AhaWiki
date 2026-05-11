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
import logics.security.UriAttackDetector
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
  private val accessLogSampleRate = applicationConf.AhaWiki.accessLog.sampleRate().max(0.0).min(1.0)

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

  private def shouldSkipAccessLogUri(uri: String): Boolean = {
    uri.startsWith("/public/")
  }

  private def shouldInsertAccessLog(status: Int): Boolean = {
    if (300 <= status && status < 400) {
      false
    } else if (400 <= status) {
      true
    } else {
      Random.nextDouble() <= accessLogSampleRate
    }
  }

  private def insertAccessLogIfSiteFound(
    site: Site,
    user: Option[Long],
    ipDeny: Option[Long],
    method: String,
    scheme: String,
    host: String,
    uri: String,
    remoteAddress: String,
    userAgent: String,
    status: Int,
    duration: Int,
  )(implicit connection: Connection): Option[Long] = {
    if (site.isNotFound) {
      logger.warn(s"Skip AccessLog insert: site not found for host=$host, uri=$uri, remoteAddress=$remoteAddress")
      None
    } else {
      models.tables.AccessLog.insert(
        site.seq,
        user,
        ipDeny,
        method,
        scheme,
        host,
        uri,
        remoteAddress,
        userAgent,
        status,
        duration
      )
    }
  }

  override def apply(nextFilter: RequestHeader => Future[Result])(requestHeader: RequestHeader): Future[Result] = {
    val startTime = System.currentTimeMillis
    val scheme = requestHeader.scheme
    val host = requestHeader.host
    val uri: String = requestHeader.uri
    val url = s"$scheme://$host$uri"
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
        if (!shouldSkipAccessLogUri(uri)) {
          database.withConnection { implicit connection =>
            insertAccessLogIfSiteFound(
            site,
            getUserSeq(requestHeader),
            optionIpDeny.map(_.seq),
            requestHeader.method,
            scheme,
            host,
            uri,
            remoteAddress,
            userAgent,
            Results.Forbidden.header.status,
            duration.toInt
            )
          }
        }
        Future(Results.Forbidden)
      })
    } else if (UriAttackDetector.isAttack(uri)) {
      logger.warn(s"${requestHeader.method}\t\tAttack\t$remoteAddress\t$url\t$userAgent")
      after((Random.nextInt(10 * 60) + 60).seconds, actorSystem.scheduler)({
        val endTime = System.currentTimeMillis
        val duration = endTime - startTime
        logRequest(requestHeader.method, 403, duration, remoteAddress, url, userAgent)
        if (!shouldSkipAccessLogUri(uri)) {
          database.withConnection { implicit connection =>
            val accessLogSeq = insertAccessLogIfSiteFound(
            site,
            getUserSeq(requestHeader),
            None,
            requestHeader.method,
            scheme,
            host,
            uri,
            remoteAddress,
            userAgent,
            Results.Forbidden.header.status,
            duration.toInt
          )
            models.tables.IpDeny.insert(remoteAddress, accessLogSeq, s"$scheme://$host$uri")
          }
        }
        Future(Results.Forbidden)
      })
    } else {
      nextFilter(requestHeader).map(result => {
        val endTime = System.currentTimeMillis
        val duration = endTime - startTime
        logRequest(requestHeader.method, result.header.status, duration, remoteAddress, url, userAgent)
        if (shouldSkipAccessLogUri(uri)) {
          logger.debug(s"Skip AccessLog insert: uri=$uri")
        } else if (shouldInsertAccessLog(result.header.status)) {
          database.withConnection { implicit connection =>
            insertAccessLogIfSiteFound(
              site,
              getUserSeq(requestHeader),
              None,
              requestHeader.method,
              scheme,
              host,
              uri,
              remoteAddress,
              userAgent,
              result.header.status,
              duration.toInt
            )
          }
        } else {
          logger.debug(s"Skip AccessLog insert by policy: status=${result.header.status}")
        }
        result.withHeaders("Request-Time" -> duration.toString)
      })
    }
  }

  private def getUserSeq(requestHeader: RequestHeader): Option[Long] = {
    SessionLogic.getUser(requestHeader).map(_.seq)
  }
}
