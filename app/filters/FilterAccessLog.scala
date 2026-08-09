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

  private def logRequest(method: String, status: Int, duration: Long, remoteAddress: String, uri: String, url: String, userAgent: String): Unit = {
    if (shouldLogAccessLog(status, uri)) {
      logger.info(Seq(f"${duration}%,12dms", method.padRight(7), status, remoteAddress.padRight(15), url, userAgent).mkString("\t"))
    }
  }

  private def shouldSkipAccessLogUri(uri: String): Boolean =
    uri.startsWith("/public/") || uri.startsWith("/assets/")

  private def shouldLogAccessLog(status: Int, uri: String): Boolean =
    !shouldSkipAccessLogUri(uri) && (status >= 400 || Random.nextDouble() <= accessLogSampleRate)

  private def shouldInsertAccessLog(status: Int): Boolean =
    if (300 <= status && status < 400) false
    else if (400 <= status)            true
    else                               Random.nextDouble() <= accessLogSampleRate

  private def makeInsert(status: Int, duration: Int, ipDenySeq: Option[Long] = None)
                        (implicit site: Site, rh: RequestHeader): ActorAccessLog.Insert =
    ActorAccessLog.Insert(
      site = site,
      user = SessionLogic.getUser(rh).map(_.seq),
      ipDeny = ipDenySeq,
      method = rh.method,
      scheme = rh.scheme,
      host = rh.host,
      path = rh.uri,
      remoteAddress = rh.remoteAddressWithXRealIp,
      userAgent = rh.userAgent.getOrElse(""),
      status = status,
      duration = duration,
    )

  private def enqueue(insert: ActorAccessLog.Insert): Unit =
    actorAccessLog ! insert

  private def enqueueAndDeny(insert: ActorAccessLog.Insert, reason: String): Unit =
    actorAccessLog ! ActorAccessLog.InsertAndDeny(insert, reason)

  private def rejectWithTarpit(label: String, maxExtraMin: Int, startTime: Long)
                               (onLog: Int => Unit)
                               (implicit site: Site, rh: RequestHeader): Future[Result] = {
    val url = s"${rh.scheme}://${rh.host}${rh.uri}"
    logger.warn(s"\t\t${rh.method}\t$FORBIDDEN\t${rh.remoteAddressWithXRealIp}\t$label\t$url\t${rh.userAgent.getOrElse("")}")
    after((Random.nextInt(maxExtraMin * 60) + 60).seconds, actorSystem.scheduler)({
      val duration = (System.currentTimeMillis - startTime).toInt
      logRequest(rh.method, FORBIDDEN, duration, rh.remoteAddressWithXRealIp, rh.uri, url, rh.userAgent.getOrElse(""))
      if (!shouldSkipAccessLogUri(rh.uri)) onLog(duration)
      Future(Results.Forbidden)
    })
  }

  private def rejectImmediately(label: String, startTime: Long)
                                (onLog: Int => Unit)
                                (implicit site: Site, rh: RequestHeader): Future[Result] = {
    val url = s"${rh.scheme}://${rh.host}${rh.uri}"
    val duration = (System.currentTimeMillis - startTime).toInt
    logger.warn(s"\t\t${rh.method}\t$FORBIDDEN\t${rh.remoteAddressWithXRealIp}\t$label\t$url\t${rh.userAgent.getOrElse("")}")
    logRequest(rh.method, FORBIDDEN, duration, rh.remoteAddressWithXRealIp, rh.uri, url, rh.userAgent.getOrElse(""))
    if (!shouldSkipAccessLogUri(rh.uri)) onLog(duration)
    Future.successful(Results.Forbidden)
  }

  override def apply(nextFilter: RequestHeader => Future[Result])(requestHeader: RequestHeader): Future[Result] = {
    val startTime              = System.currentTimeMillis
    implicit val rh: RequestHeader = requestHeader
    val remoteAddress          = rh.remoteAddressWithXRealIp
    val uri                    = rh.uri
    val url                    = s"${rh.scheme}://${rh.host}$uri"

    val isBannedInMemory = ipRateLimiter.isKnownBanned(remoteAddress)
    val isCleanInMemory  = !isBannedInMemory && ipRateLimiter.isKnownClean(remoteAddress)
    // SiteLogic.get은 AhaWikiCacheMemoryDomainSite(메모리)만 사용하므로 connection 불필요
    implicit val site: Site = SiteLogic.get(rh.host)
    val optionIpDeny: Option[IpDeny] =
      if (isBannedInMemory || isCleanInMemory) None
      else database.withConnection { implicit connection =>
        val found = models.tables.IpDeny.selectLatest(remoteAddress)
        if (found.isDefined) ipRateLimiter.ban(remoteAddress)
        else ipRateLimiter.markClean(remoteAddress)
        found
      }

    if (isBannedInMemory || optionIpDeny.isDefined) {
      val label = if (isBannedInMemory) "IpDeny:Cache" else "IpDeny:DB"
      rejectWithTarpit(label, maxExtraMin = 5, startTime) { duration =>
        enqueue(makeInsert(FORBIDDEN, duration, optionIpDeny.map(_.seq)))
      }
    } else if (UriAttackDetector.isAttack(uri)) {
      ipRateLimiter.ban(remoteAddress)
      rejectWithTarpit("UriAttack", maxExtraMin = 10, startTime) { duration =>
        enqueueAndDeny(makeInsert(FORBIDDEN, duration), url)
      }
    } else if (!applicationConf.AhaWiki.ipWhitelist().contains(remoteAddress) && ipRateLimiter.recordAndCheck(remoteAddress, uri)) {
      rejectImmediately("RateLimit", startTime) { duration =>
        enqueueAndDeny(makeInsert(FORBIDDEN, duration), s"RateLimit:$remoteAddress")
      }
    } else {
      nextFilter(requestHeader).map { result =>
        val duration = (System.currentTimeMillis - startTime).toInt
        logRequest(rh.method, result.header.status, duration, remoteAddress, uri, url, rh.userAgent.getOrElse(""))
        if (shouldSkipAccessLogUri(uri))
          logger.debug(s"Skip AccessLog insert: uri=$uri")
        else if (shouldInsertAccessLog(result.header.status))
          enqueue(makeInsert(result.header.status, duration))
        else
          logger.debug(s"Skip AccessLog insert by policy: status=${result.header.status}")
        result.withHeaders("Request-Time" -> duration.toString)
      }
    }
  }
}
