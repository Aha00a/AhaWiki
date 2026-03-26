package logics

import models.tables.User
import play.api.mvc.Request
import play.api.mvc.RequestHeader
import play.api.mvc.Session

object SessionLogic {
  val sessionKeySeq = "seq"
  val sessionKeyEmail = "email"
  val sessionKeyNickname = "nickname"

  def getUser(request: RequestHeader): Option[User.IdEmailNickname] = {
    for {
      seq <- request.session.get(sessionKeySeq).flatMap(_.toLongOption)
      email <- request.session.get(sessionKeyEmail)
      nickname <- request.session.get(sessionKeyNickname)
    } yield User.IdEmailNickname(seq, email, nickname)
  }

  def login(request: Request[Any], user: User.IdEmailNickname): Session = {
    request.session +
      (sessionKeySeq -> user.seq.toString) +
      (sessionKeyEmail -> user.email) +
      (sessionKeyNickname -> user.nickname)
  }
}
