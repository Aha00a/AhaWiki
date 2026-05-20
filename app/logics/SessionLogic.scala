package logics

import models.tables.User
import play.api.mvc.Request
import play.api.mvc.RequestHeader
import play.api.mvc.Session

object SessionLogic {
  val sessionKeySeq = "seq"
  val sessionKeyLoginEmail = "loginEmail"
  val sessionKeyLegacyEmail = "email"
  val sessionKeyNickname = "nickname"
  val sessionKeyProfileImageUrl = "profileImageUrl"
  val sessionKeyPendingMergeEmail = "pendingMergeEmail"

  def getUser(request: RequestHeader): Option[User.SessionUser] = {
    for {
      seq <- request.session.get(sessionKeySeq).flatMap(_.toLongOption)
      nickname <- request.session.get(sessionKeyNickname)
    } yield User.SessionUser(
      seq = seq,
      nickname = nickname,
      loginEmail = request.session.get(sessionKeyLoginEmail).orElse(request.session.get(sessionKeyLegacyEmail)).filter(_.nonEmpty),
    )
  }

  def getUserProfileImageUrl(request: RequestHeader): Option[String] =
    request.session.get(sessionKeyProfileImageUrl).filter(_.nonEmpty)

  def login(request: Request[Any], user: User.SessionUser, profileImageUrl: Option[String] = None): Session = {
    val baseSession = request.session +
      (sessionKeySeq -> user.seq.toString) +
      (sessionKeyNickname -> user.nickname) -
      sessionKeyLegacyEmail

    val sessionWithLoginEmail = user.loginEmail.filter(_.nonEmpty) match {
      case Some(email) => baseSession + (sessionKeyLoginEmail -> email)
      case None => baseSession - sessionKeyLoginEmail
    }

    profileImageUrl.filter(_.nonEmpty) match {
      case Some(url) => sessionWithLoginEmail + (sessionKeyProfileImageUrl -> url)
      case None => sessionWithLoginEmail - sessionKeyProfileImageUrl
    }
  }
}
