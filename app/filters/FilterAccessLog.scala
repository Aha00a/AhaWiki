package filters

import akka.stream.Materializer
import com.aha00a.commons.Implicits._
import com.aha00a.play.Implicits._
import logics.SessionLogic
import models.tables.Site
import play.api.Logging
import play.api.mvc._

import java.sql.Connection
import javax.inject.Inject
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}


class FilterAccessLog @Inject()(
    implicit val
    mat: Materializer,
    ec: ExecutionContext,
    database: play.api.db.Database
) extends Filter with Logging {
  private def isUriAttack(uri: String): Boolean = uri.startsWith("/wp-") || Seq("/wp-login.php").contains(uri)

  override def apply(nextFilter: RequestHeader => Future[Result])(requestHeader: RequestHeader): Future[Result] = {
    val startTime = System.currentTimeMillis
    val scheme = requestHeader.scheme
    val host = requestHeader.host
    val uri: String = requestHeader.uri
    val url = s"$scheme://$host$uri"
    val remoteAddress = requestHeader.remoteAddressWithXRealIp
    val userAgent = requestHeader.userAgent.getOrElse("")
    val ipDeny = database.withConnection { implicit connection => models.tables.IpDeny.selectLatest(remoteAddress) }

    if (ipDeny.isDefined) {
      Thread.sleep(60.seconds.toMillis);
      val endTime = System.currentTimeMillis
      val duration = endTime - startTime
      logger.info(Seq(
        requestHeader.method.padRight(7),
        Results.Forbidden.header.status,
        s"${duration}ms".padLeft(7),
        remoteAddress.padRight(15),
        url,
        userAgent,
      ).mkString("\t"))
      database.withConnection { implicit connection =>
        implicit val site: Site = Site.selectWhereDomain(host).getOrElse(Site(-1, ""))
        models.tables.AccessLog.insert(
          site.seq,
          getUserSeq(requestHeader),
          ipDeny.map(_.seq),
          requestHeader.method,
          scheme,
          host,
          uri,
          url,
          remoteAddress,
          userAgent,
          Results.Forbidden.header.status,
          duration.toInt,
        )
      }
      Future(Results.Forbidden)
    } else if (isUriAttack(uri)) {
      Thread.sleep(60.seconds.toMillis);
      val endTime = System.currentTimeMillis
      val duration = endTime - startTime
      logger.info(Seq(
        requestHeader.method.padRight(7),
        403,
        s"${duration}ms".padLeft(7),
        remoteAddress.padRight(15),
        url,
        userAgent,
      ).mkString("\t"))
      database.withConnection { implicit connection =>
        implicit val site: Site = Site.selectWhereDomain(host).getOrElse(Site(-1, ""))
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
          duration.toInt,
        )
        models.tables.IpDeny.insert(remoteAddress, accessLogSeq, s"$scheme://$host$uri")
      }
      Future(Results.Forbidden)
    } else {
      nextFilter(requestHeader).map(result => {
        val endTime = System.currentTimeMillis
        val duration = endTime - startTime
        logger.info(Seq(
          requestHeader.method.padRight(7),
          result.header.status,
          s"${duration}ms".padLeft(7),
          remoteAddress.padRight(15),
          url,
          userAgent,
        ).mkString("\t"))
        database.withConnection { implicit connection =>
          implicit val site: Site = Site.selectWhereDomain(host).getOrElse(Site(-1, ""))
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
            duration.toInt,
          )
        }
        result.withHeaders("Request-Time" -> duration.toString)
      })
    }
  }

  private def getUserSeq(requestHeader: RequestHeader)(implicit connection: Connection): Option[Long] = {
    SessionLogic
      .getId(requestHeader)
      .flatMap(models.tables.User.selectWhereEmail)
      .map(_.seq)
  }
}
