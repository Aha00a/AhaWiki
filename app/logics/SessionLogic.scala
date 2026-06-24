package logics

import models.tables.User
import models.tables.UserApiKey
import models.tables.UserEmail
import play.api.db.Database
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

  def getApiKeyUser(request: RequestHeader)(implicit database: Database): Option[User.SessionUser] = {
    val rawKey = request.headers
      .get("Authorization")
      .map(_.trim)
      .collect {
        case value if value.regionMatches(true, 0, "Bearer ", 0, "Bearer ".length) =>
          value.drop("Bearer ".length).trim
      }
      .filter(_.nonEmpty)

    rawKey.flatMap { key =>
      database.withConnection { implicit connection =>
        UserApiKey.selectByHash(UserApiKey.hash(key)).flatMap { apiKey =>
          UserApiKey.touchLastUsed(apiKey.seq)
          User.selectBySeq(apiKey.user).map { user =>
            user.toSessionUser(UserEmail.selectPrimaryEmailByUser(user.seq))
          }
        }
      }
    }
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
