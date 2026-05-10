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

case class User(seq: Long, created: LocalDateTime, updated: LocalDateTime, email: String, nickname: String, profileImageUrl: Option[String]) {
  def toIdEmailNickname: User.IdEmailNickname = User.IdEmailNickname(seq, email, nickname)
}

object User {
  case class IdEmailNickname(seq: Long, email: String, nickname: String)

  //noinspection TypeAnnotation
  def tupled = (apply _).tupled

  def selectWhereEmail(email: String)(implicit connection: Connection): Option[User] = {
      SQL"""
        SELECT
            U.seq, U.created, U.updated, U.email, U.nickname, U.profileImageUrl
            FROM User U
            WHERE U.email = $email
         """
      .as(long("seq") ~ localDateTime("created") ~ localDateTime("updated") ~ str("email") ~ str("nickname") ~ str("profileImageUrl").? singleOpt).map(flatten)
      .map(User.tupled)
  }


  def selectBySeq(seq: Long)(implicit connection: Connection): Option[User] = {
    SQL"""
      SELECT
          U.seq, U.created, U.updated, U.email, U.nickname, U.profileImageUrl
          FROM User U
          WHERE U.seq = $seq
     """
      .as(long("seq") ~ localDateTime("created") ~ localDateTime("updated") ~ str("email") ~ str("nickname") ~ str("profileImageUrl").? singleOpt).map(flatten)
      .map(User.tupled)
  }

  def selectByNickname(nickname: String)(implicit connection: Connection): Option[User] = {
    SQL"""
      SELECT
          U.seq, U.created, U.updated, U.email, U.nickname, U.profileImageUrl
          FROM User U
          WHERE U.nickname = $nickname
     """
      .as(long("seq") ~ localDateTime("created") ~ localDateTime("updated") ~ str("email") ~ str("nickname") ~ str("profileImageUrl").? singleOpt).map(flatten)
      .map(User.tupled)
  }

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
          SQL"""INSERT INTO User (email, nickname, profileImageUrl) VALUES ($email, $nickname, $profileImageUrl)"""
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

  def selectOrInsert(email: String, profileImageUrl: Option[String])(implicit connection: Connection): Option[IdEmailNickname] = {
    selectWhereEmail(email)
      .map { user =>
        if (profileImageUrl.isDefined && user.profileImageUrl != profileImageUrl) {
          SQL"""UPDATE User SET profileImageUrl = $profileImageUrl WHERE seq = ${user.seq}""".executeUpdate()
        }
        user.toIdEmailNickname
      }
      .orElse(insert(email, profileImageUrl).map {
        case (id, nickname) => IdEmailNickname(id, email, nickname)
      })
  }
}
