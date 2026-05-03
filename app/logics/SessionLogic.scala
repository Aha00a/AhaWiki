package logics

import models.tables.User
import play.api.mvc.Request
import play.api.mvc.RequestHeader
import play.api.mvc.Session

object SessionLogic {
  val sessionKeySeq = "seq"
  val sessionKeyEmail = "email"
  val sessionKeyNickname = "nickname"
  val sessionKeyProfileImageUrl = "profileImageUrl"

  def getUser(request: RequestHeader): Option[User.IdEmailNickname] = {
    for {
      seq <- request.session.get(sessionKeySeq).flatMap(_.toLongOption)
      email <- request.session.get(sessionKeyEmail)
      nickname <- request.session.get(sessionKeyNickname)
    } yield User.IdEmailNickname(seq, email, nickname)
  }

  def getUserProfileImageUrl(request: RequestHeader): Option[String] =
    request.session.get(sessionKeyProfileImageUrl).filter(_.nonEmpty)

  def login(request: Request[Any], user: User.IdEmailNickname, profileImageUrl: Option[String] = None): Session = {
    val baseSession = request.session +
      (sessionKeySeq -> user.seq.toString) +
      (sessionKeyEmail -> user.email) +
      (sessionKeyNickname -> user.nickname)

    profileImageUrl.filter(_.nonEmpty) match {
      case Some(url) => baseSession + (sessionKeyProfileImageUrl -> url)
      case None => baseSession - sessionKeyProfileImageUrl
    }
  }
}
