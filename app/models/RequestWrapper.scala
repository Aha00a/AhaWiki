package models

import com.aha00a.play.Implicits.RichRequest
import logics.SessionLogic
import play.api.mvc.Request

import java.util.Locale

trait RequestWrapper {
  def getId: Option[String]

  def locale: Locale

  def getQueryString(key: String): Option[String]

  val remoteAddress: String

  def flashGet(key: String): Option[String]

  def host: String
}

object RequestWrapper {
  def apply()(implicit request: Request[Any]): RequestWrapper = new RequestWrapper {
    override def getId: Option[String] = SessionLogic.getId(request)

    override def locale: Locale = request.locale

    override def getQueryString(key: String): Option[String] = request.getQueryString(key)

    override val remoteAddress: String = request.remoteAddressWithXRealIp

    override def flashGet(key: String): Option[String] = request.flash.get(key)

    override def host: String = request.host
  }

  def empty: RequestWrapper = new RequestWrapper {
    override def getId: Option[String] = None

    override def locale: Locale = Locale.KOREA

    override def getQueryString(key: String): Option[String] = None

    override val remoteAddress: String = "127.0.0.1"

    override def flashGet(key: String): Option[String] = None

    override def host: String = ""
  }
}
