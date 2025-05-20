package logics

import play.api.mvc.Request
import play.api.mvc.RequestHeader
import play.api.mvc.Session

object SessionLogic {
  val sessionKeyEmail = "email"
  val sessionKeyUser = "user"

  def getId(request: RequestHeader): Option[String] = {
    request.session.get(sessionKeyEmail)
  }

  def getUser(request: RequestHeader): Option[Int] = {
    request.session.get(sessionKeyUser).map(_.toInt)
  }

  def login(request: Request[Any], email: String, user: Int): Session = {
    request.session + (sessionKeyEmail -> email) + (sessionKeyUser -> user.toString)
  }
}
