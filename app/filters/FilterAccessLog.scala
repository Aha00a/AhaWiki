package filters

import akka.stream.Materializer
import com.aha00a.commons.Implicits._
import com.aha00a.play.Implicits._
import javax.inject.Inject
import play.api.Logger
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
      if(uri.isNotNullOrEmpty && !uri.startsWith("/public/"))
        logger.info(Seq(requestHeader.remoteAddressWithXRealIp.padRight(15), s"${duration}ms".padLeft(7), result.header.status, requestHeader.method, uri, requestHeader.userAgent.getOrElse("")).mkString("\t"))

      result.withHeaders("Request-Time" -> duration.toString)
    })
  }
}