package models.tables

import anorm.SqlParser.flatten
import anorm.SqlParser.long
import anorm.SqlParser.scalar
import anorm.SqlParser.str
import anorm._
import com.aha00a.play.AnormSqlParser.localDateTime

import java.sql.Connection
import java.time.LocalDateTime
import scala.annotation.tailrec

case class User(seq: Long, created: LocalDateTime, updated: LocalDateTime, nickname: String, profileImageUrl: Option[String]) {
  def toSessionUser(loginEmail: Option[String]): User.SessionUser = User.SessionUser(seq, nickname, loginEmail)
}

object User {
  case class SessionUser(seq: Long, nickname: String, loginEmail: Option[String])

  //noinspection TypeAnnotation
  def tupled = (apply _).tupled

  def selectWhereEmail(email: String)(implicit connection: Connection): Option[User] = {
    UserEmail.selectByEmail(email).flatMap(userEmail => selectBySeq(userEmail.user))
  }

  def selectBySeq(seq: Long)(implicit connection: Connection): Option[User] = {
    SQL"""
      SELECT
          U.seq, U.created, U.updated, U.nickname, U.profileImageUrl
          FROM User U
          WHERE U.seq = $seq
     """
      .as(long("seq") ~ localDateTime("created") ~ localDateTime("updated") ~ str("nickname") ~ str("profileImageUrl").? singleOpt).map(flatten)
      .map(User.tupled)
  }

  def selectByNickname(nickname: String)(implicit connection: Connection): Option[User] = {
    SQL"""
      SELECT
          U.seq, U.created, U.updated, U.nickname, U.profileImageUrl
          FROM User U
          WHERE U.nickname = $nickname
     """
      .as(long("seq") ~ localDateTime("created") ~ localDateTime("updated") ~ str("nickname") ~ str("profileImageUrl").? singleOpt).map(flatten)
      .map(User.tupled)
  }

  /**
   * Renames a user and stamps when it happened.
   *
   * Returns 0 rather than throwing when the name is taken only if the caller checked first;
   * `User_nickname_lower_uindex` is what actually decides, and a violation surfaces as the
   * exception it is. The check before this call is for a useful message, not for correctness.
   */
  def updateNickname(seq: Long, nickname: String)(implicit connection: Connection): Int =
    SQL"""
      UPDATE User
      SET nickname = $nickname, dateNicknameChanged = ${LocalDateTime.now()}
      WHERE seq = $seq
    """.executeUpdate()

  def insert(email: String, profileImageUrl: Option[String])(implicit connection: Connection): Option[(Long, String)] = {
    val baseNickname = {
      val localPart = email.takeWhile(_ != '@').toLowerCase
      val normalized = localPart
        .map(ch => if (ch.isLetterOrDigit || ch == '_') ch else '_')
        .mkString
        .replaceAll("_+", "_")
        .stripPrefix("_")
        .stripSuffix("_")
        .take(255)
      if (normalized.nonEmpty) normalized else "user"
    }

    def generateSuffix(): String =
      scala.util.Random.alphanumeric
        .filter(_.isLetterOrDigit)
        .map(_.toLower)
        .take(10)
        .mkString

    def generateNickname(attempt: Int): String =
      if (attempt == 0) baseNickname
      else {
        val suffix = generateSuffix()
        val prefixMaxLength = 255 - (suffix.length + 1)
        val prefix = baseNickname.take(prefixMaxLength.max(1))
        s"${prefix}_${suffix}"
      }

    @tailrec
    def tryInsert(attempt: Int): Option[(Long, String)] = {
      if (attempt >= 100)
        None
      else {
        val nickname = generateNickname(attempt)
        try {
          SQL"""INSERT INTO User (nickname, profileImageUrl) VALUES ($nickname, $profileImageUrl)"""
            .executeInsert(scalar[Long].singleOpt)
            .map(id => (id, nickname))
        } catch {
          case _: java.sql.SQLIntegrityConstraintViolationException =>
            tryInsert(attempt + 1)
        }
      }
    }

    tryInsert(0)
  }

  def selectOrInsert(email: String, profileImageUrl: Option[String])(implicit connection: Connection): Option[SessionUser] = {
    selectWhereEmail(email)
      .map { user =>
        if (profileImageUrl.isDefined && user.profileImageUrl != profileImageUrl) {
          SQL"""UPDATE User SET profileImageUrl = $profileImageUrl WHERE seq = ${user.seq}""".executeUpdate()
        }
        user.toSessionUser(Some(email))
      }
      .orElse {
        try {
          LocalTransaction {
            insert(email, profileImageUrl).map {
              case (id, nickname) =>
                UserEmail.insert(id, email, isPrimary = true)
                SessionUser(id, nickname, Some(email))
            }
          }
        } catch {
          case _: java.sql.SQLIntegrityConstraintViolationException =>
            selectWhereEmail(email).map(_.toSessionUser(Some(email)))
        }
      }
  }
}
