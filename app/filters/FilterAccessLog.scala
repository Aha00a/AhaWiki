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
          requestHeader.method.padRight(7),
          result.header.status,
          s"${duration}ms".padLeft(7),
          requestHeader.remoteAddressWithXRealIp.padRight(15),
          requestHeader.host + uri,
          userAgent,
        ).mkString("\t"))
      }

      result.withHeaders("Request-Time" -> duration.toString)
    })
  }
}
