package logics.wikis

import java.net.URLEncoder

import play.api.mvc.Request
import utils.CookieUtil

object RecentlyVisited {
  val separator = "___"
  val cookieKey = "RecentlyVisited"
  val count = 10

  def value(request:Request[Any]):Seq[String] = {
    CookieUtil.value(request, cookieKey, "").split(separator)
  }

  def valueStr(request:Request[Any], name:String):String = {
    value(request, URLEncoder.encode(name, "UTF-8")).mkString(separator)
  }

  def value(request: Request[Any], name: String): Seq[String] = {
    (name +: value(request).filter(_ != name)).take(count)
  }
}
