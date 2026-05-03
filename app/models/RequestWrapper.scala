package models

import com.aha00a.play.Implicits.RichRequest
import logics.SessionLogic
import models.tables.User
import play.api.mvc.RequestHeader

import java.util.Locale

trait RequestWrapper {
  def getUser: Option[User.IdEmailNickname]
  def getUserProfileImageUrl: Option[String]
  def locale: Locale
  def getQueryString(key: String): Option[String]
  val remoteAddress: String
  def flashGet(key: String): Option[String]
  def host: String
}

object RequestWrapper {
  def apply()(implicit requestHeader: RequestHeader): RequestWrapper = new RequestWrapper {
    override def getUser: Option[User.IdEmailNickname] = SessionLogic.getUser(requestHeader)
    override def getUserProfileImageUrl: Option[String] = SessionLogic.getUserProfileImageUrl(requestHeader)
    override def locale: Locale = requestHeader.locale
    override def getQueryString(key: String): Option[String] = requestHeader.getQueryString(key)
    override val remoteAddress: String = requestHeader.remoteAddressWithXRealIp
    override def flashGet(key: String): Option[String] = requestHeader.flash.get(key)
    override def host: String = requestHeader.host
  }

  def empty: RequestWrapper = new RequestWrapper {
    override def getUser: Option[User.IdEmailNickname] = None
    override def getUserProfileImageUrl: Option[String] = None
    override def locale: Locale = Locale.KOREA
    override def getQueryString(key: String): Option[String] = None
    override val remoteAddress: String = "127.0.0.1"
    override def flashGet(key: String): Option[String] = None
    override def host: String = ""
  }
}
