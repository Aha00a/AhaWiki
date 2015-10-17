package logics

import play.api.mvc.{AnyContent, Request, Session}

object SessionLogic {
  val sessionKeyId = "id"

  def getId(request: Request[AnyContent]): Option[String] = {
    request.session.get(sessionKeyId)
  }

  def login(request: Request[AnyContent], id: String): Session = {
    request.session + (sessionKeyId -> id)
  }
}
