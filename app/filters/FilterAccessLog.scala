package filters

import akka.stream.Materializer
import com.aha00a.commons.Implicits._
import com.aha00a.play.Implicits._
import models.tables.Site
import play.api.Logging
import play.api.mvc._

import javax.inject.Inject
import scala.collection.immutable.Seq
import scala.concurrent.{ExecutionContext, Future}


class FilterAccessLog @Inject()(
    implicit val
    mat: Materializer,
    ec: ExecutionContext,
    database: play.api.db.Database
) extends Filter with Logging {
  val seqRemoteAddressBlocked: Seq[String] = Seq("13.59.169.75")
  override def apply(nextFilter: RequestHeader => Future[Result])(requestHeader: RequestHeader): Future[Result] = {
    val startTime = System.currentTimeMillis
    val scheme = requestHeader.scheme
    val host = requestHeader.host
    val uri: String = requestHeader.uri
    val remoteAddress = requestHeader.remoteAddressWithXRealIp
    val userAgent = requestHeader.userAgent.getOrElse("")
    if(seqRemoteAddressBlocked.contains(remoteAddress)) {
      val endTime = System.currentTimeMillis
      val duration = endTime - startTime
      logger.info(Seq(
        requestHeader.method.padRight(7),
        403,
        s"${duration}ms".padLeft(7),
        remoteAddress.padRight(15),
        s"$scheme://$host$uri",
        userAgent,
      ).mkString("\t"))
      database.withConnection { implicit connection =>
        implicit val site: Site = Site.selectWhereDomain(host).getOrElse(Site(-1, ""))
        models.tables.AccessLog.insert(
          site.seq,
          requestHeader.method,
          uri,
          remoteAddress,
          userAgent,
          0,
          0,
        )
      }
      Future(Results.Forbidden)
    } else {
      nextFilter(requestHeader).map(result => {
        val endTime = System.currentTimeMillis
        val duration = endTime - startTime
        if (
          uri.isNotNullOrEmpty &&
            !uri.startsWith("/public/") &&
            userAgent != "Mozilla/5.0+(compatible; UptimeRobot/2.0; http://www.uptimerobot.com/)"
        ) {
          logger.info(Seq(
            requestHeader.method.padRight(7),
            result.header.status,
            s"${duration}ms".padLeft(7),
            remoteAddress.padRight(15),
            s"$scheme://$host$uri",
            userAgent,
          ).mkString("\t"))
          database.withConnection { implicit connection =>
            implicit val site: Site = Site.selectWhereDomain(host).getOrElse(Site(-1, ""))
            models.tables.AccessLog.insert(
              site.seq,
              requestHeader.method,
              uri,
              remoteAddress,
              userAgent,
              result.header.status,
              duration.toInt,
            )
          }
        }
        result.withHeaders("Request-Time" -> duration.toString)
      })
    }
  }
}
