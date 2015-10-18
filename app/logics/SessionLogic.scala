package logics

import play.api.mvc.{Request, Session}

object SessionLogic {
  val sessionKeyId = "id"

  def getId(request: Request[Any]): Option[String] = {
    request.session.get(sessionKeyId)
  }

  def login(request: Request[Any], id: String): Session = {
    request.session + (sessionKeyId -> id)
  }
}
