package filters

import akka.stream.Materializer
import com.aha00a.commons.Implicits._
import com.aha00a.play.Implicits._
import javax.inject.Inject
import play.api.Logging
import play.api.mvc._

import scala.concurrent.{ExecutionContext, Future}

class FilterAccessLog @Inject()(implicit val mat: Materializer, ec: ExecutionContext) extends Filter with Logging {
  override def apply(nextFilter: RequestHeader => Future[Result])(requestHeader: RequestHeader): Future[Result] = {
    val startTime = System.currentTimeMillis
    nextFilter(requestHeader).map(result => {
      val endTime = System.currentTimeMillis
      val duration = endTime - startTime
      val uri: String = requestHeader.uri
      val userAgent = requestHeader.userAgent.getOrElse("")
      if(
        uri.isNotNullOrEmpty &&
        !uri.startsWith("/public/") &&
        userAgent != "Mozilla/5.0+(compatible; UptimeRobot/2.0; http://www.uptimerobot.com/)"
      ) {
        logger.info(Seq(
          requestHeader.host.padRight(25),
          requestHeader.remoteAddressWithXRealIp.padRight(15),
          s"${duration}ms".padLeft(7),
          requestHeader.method,
          result.header.status,
          uri,
          userAgent,
        ).mkString("\t"))
      }

      result.withHeaders("Request-Time" -> duration.toString)
    })
  }
}
