package filters

import actors.ActorAccessLog
import org.apache.pekko.actor.ActorRef
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.pattern.after
import org.apache.pekko.stream.Materializer
import com.aha00a.commons.Implicits._
import com.aha00a.play.Implicits._
import logics.AhaWikiCache
import logics.ApplicationConf
import logics.SessionLogic
import logics.SiteLogic
import logics.security.IpRateLimiter
import logics.security.UriAttackDetector
import models.tables.IpDeny
import models.tables.Site
import play.api.Environment
import play.api.Logging
import play.api.db.Database
import play.api.http.Status.FORBIDDEN
import play.api.libs.ws.WSClient
import play.api.mvc._

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
  @Named("access-log-actor") actorAccessLog: ActorRef,
  applicationConf: ApplicationConf,
  ahaWikiCache: AhaWikiCache,
  wsClient: WSClient,
  executionContext: ExecutionContext,
  ipRateLimiter: IpRateLimiter,
) extends Filter with Logging {
  private val accessLogSampleRate = applicationConf.AhaWiki.accessLog.sampleRate().max(0.0).min(1.0)

  private def logRequest(method: String, status: Int, duration: Long, remoteAddress: String, url: String, userAgent: String): Unit = {
    logger.info(Seq(
      f"${duration}%,12dms",
      method.padRight(7),
      status,
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

  private def enqueueAccessLog(
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
  ): Unit = {
    actorAccessLog ! ActorAccessLog.Insert(
      site,
      user,
      ipDeny,
      method,
      scheme,
      host,
      uri,
      remoteAddress,
      userAgent,
      status,
      duration,
    )
  }

  private def enqueueAccessLogAndDeny(insert: ActorAccessLog.Insert, reason: String): Unit = {
    actorAccessLog ! ActorAccessLog.InsertAndDeny(insert, reason)
  }

  override def apply(nextFilter: RequestHeader => Future[Result])(requestHeader: RequestHeader): Future[Result] = {
    val startTime = System.currentTimeMillis
    val scheme = requestHeader.scheme
    val host = requestHeader.host
    val uri: String = requestHeader.uri
    val url = s"$scheme://$host$uri"
    val remoteAddress = requestHeader.remoteAddressWithXRealIp
    val userAgent = requestHeader.userAgent.getOrElse("")
    val isBannedInMemory = ipRateLimiter.isKnownBanned(remoteAddress)
    val (optionIpDeny: Option[IpDeny], siteFound) = database.withConnection { implicit connection =>
      implicit val site: Site = SiteLogic.get(requestHeader.host)
      val ipDeny =
        if (isBannedInMemory) None
        else {
          val found = models.tables.IpDeny.selectLatest(remoteAddress)
          if (found.isDefined) ipRateLimiter.ban(remoteAddress)
          found
        }
      (ipDeny, site)
    }
    implicit val site: Site = siteFound

    if (isBannedInMemory || optionIpDeny.isDefined) {
      logger.warn(s"${requestHeader.method}\t\tAttack\t${FORBIDDEN}\t$remoteAddress\t$url\t$userAgent")
      after((Random.nextInt(5 * 60) + 60).seconds, actorSystem.scheduler)({
        val endTime = System.currentTimeMillis
        val duration = endTime - startTime
        logRequest(requestHeader.method, Results.Forbidden.header.status, duration, remoteAddress, url, userAgent)
        if (!shouldSkipAccessLogUri(uri)) {
          enqueueAccessLog(
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
        Future(Results.Forbidden)
      })
    } else if (UriAttackDetector.isAttack(uri)) {
      ipRateLimiter.ban(remoteAddress)
      logger.warn(s"${requestHeader.method}\t\tAttack\t${FORBIDDEN}\t$remoteAddress\t$url\t$userAgent")
      after((Random.nextInt(10 * 60) + 60).seconds, actorSystem.scheduler)({
        val endTime = System.currentTimeMillis
        val duration = endTime - startTime
        logRequest(requestHeader.method, 403, duration, remoteAddress, url, userAgent)
        if (!shouldSkipAccessLogUri(uri)) {
          enqueueAccessLogAndDeny(
            ActorAccessLog.Insert(
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
            ),
            s"$scheme://$host$uri",
          )
        }
        Future(Results.Forbidden)
      })
    } else if (ipRateLimiter.recordAndCheck(remoteAddress, uri)) {
      val duration = (System.currentTimeMillis - startTime).toInt
      logger.warn(s"${requestHeader.method}\t\tRateLimit\t${FORBIDDEN}\t$remoteAddress\t$url\t$userAgent")
      logRequest(requestHeader.method, FORBIDDEN, duration, remoteAddress, url, userAgent)
      enqueueAccessLogAndDeny(
        ActorAccessLog.Insert(
          site,
          getUserSeq(requestHeader),
          None,
          requestHeader.method,
          scheme,
          host,
          uri,
          remoteAddress,
          userAgent,
          FORBIDDEN,
          duration,
        ),
        s"RateLimit:$remoteAddress",
      )
      Future.successful(Results.Forbidden)
    } else {
      nextFilter(requestHeader).map(result => {
        val endTime = System.currentTimeMillis
        val duration = endTime - startTime
        logRequest(requestHeader.method, result.header.status, duration, remoteAddress, url, userAgent)
        if (shouldSkipAccessLogUri(uri)) {
          logger.debug(s"Skip AccessLog insert: uri=$uri")
        } else if (shouldInsertAccessLog(result.header.status)) {
          enqueueAccessLog(
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
            duration.toInt,
          )
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
