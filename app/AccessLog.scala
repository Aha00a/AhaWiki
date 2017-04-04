import javax.inject.Inject

import play.api.Logger
import play.api.mvc._

import scala.concurrent.{ExecutionContext, Future}

class AccessLog @Inject()(implicit ec: ExecutionContext) extends Filter {
  def apply(nextFilter: RequestHeader => Future[Result])(requestHeader: RequestHeader): Future[Result] = {
    val startTime = System.currentTimeMillis
    nextFilter(requestHeader).map(result => {
      val endTime = System.currentTimeMillis
      val duration = endTime - startTime
      Logger.info(Seq(getRemoteAddress(requestHeader).padTo(15, " ").mkString, result.header.status, requestHeader.method, requestHeader.uri, duration + "ms").mkString("\t"))
      result.withHeaders("Request-Time" -> duration.toString)
    })
  }

  def getRemoteAddress(requestHeader: RequestHeader): String = {
    requestHeader.headers.get("X-Real-IP").getOrElse(requestHeader.remoteAddress)
  }
}