package filters

import akka.stream.Materializer
import com.aha00a.commons.Implicits._
import com.aha00a.play.Implicits._
import play.api.Logging
import play.api.mvc._

import javax.inject.Inject
import scala.collection.immutable.Seq
import scala.concurrent.{ExecutionContext, Future}

val seqRemoteAddressBlocked = Seq("13.59.169.75")

class FilterAccessLog @Inject()(implicit val mat: Materializer, ec: ExecutionContext) extends Filter with Logging {
  override def apply(nextFilter: RequestHeader => Future[Result])(requestHeader: RequestHeader): Future[Result] = {
    val startTime = System.currentTimeMillis
    val remoteAddress = requestHeader.remoteAddressWithXRealIp
    val userAgent = requestHeader.userAgent.getOrElse("")
    val uri: String = requestHeader.uri
    if(seqRemoteAddressBlocked.contains(remoteAddress)) {
      val endTime = System.currentTimeMillis
      val duration = endTime - startTime
      logger.info(Seq(
        requestHeader.method.padRight(7),
        403,
        s"${duration}ms".padLeft(7),
        remoteAddress.padRight(15),
        s"${requestHeader.scheme}://${requestHeader.host}$uri",
        userAgent,
      ).mkString("\t"))
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
            s"${requestHeader.scheme}://${requestHeader.host}$uri",
            userAgent,
          ).mkString("\t"))
        }
        result.withHeaders("Request-Time" -> duration.toString)
      })
    }
  }
}
