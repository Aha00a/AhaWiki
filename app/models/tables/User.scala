package models.tables

import anorm.SqlParser.date
import anorm.SqlParser.flatten
import anorm.SqlParser.int
import anorm.SqlParser.long
import anorm.SqlParser.scalar
import anorm.SqlParser.str
import anorm._

import java.sql.Connection
import java.util.Date
import scala.annotation.tailrec

case class User(seq:Int, created: Date, updated: Date, email: String, nickname: String) {
  def toIdEmailNickname: User.IdEmailNickname = User.IdEmailNickname(seq, email, nickname)
}

object User {
  case class IdEmailNickname(seq: Int, email: String, nickname: String)

  //noinspection TypeAnnotation
  def tupled = (apply _).tupled

  def selectWhereEmail(email: String)(implicit connection: Connection): Option[User] = {
    SQL"""
        SELECT
            U.seq, U.created, U.updated, U.email, U.nickname
            FROM User U
            WHERE U.email = $email
         """
      .as(int("seq") ~ date("created") ~ date("updated") ~ str("email") ~ str("nickname") singleOpt).map(flatten)
      .map(User.tupled)
  }

  def insert(email: String)(implicit connection: Connection): Option[(Int, String)] = {
    val base = email.takeWhile(_ != '@').take(3).toLowerCase

    def generateSuffix(): String =
      scala.util.Random.alphanumeric
        .filter(_.isLetterOrDigit)
        .map(_.toLower)
        .take(10)
        .mkString

    def generateNickname(): String =
      s"${base}_${generateSuffix()}"

    @tailrec
    def tryInsert(attempt: Int): Option[(Int, String)] = {
      if (attempt >= 100)
        None
      else {
        val nickname = generateNickname()
        try {
          SQL"""INSERT INTO User (email, nickname) VALUES ($email, $nickname)"""
            .executeInsert(scalar[Int].singleOpt)
            .map(id => (id, nickname))
        } catch {
          case _: java.sql.SQLIntegrityConstraintViolationException =>
            tryInsert(attempt + 1)
        }
      }
    }

    tryInsert(0)
  }

  def selectOrInsert(email: String)(implicit connection: Connection): Option[IdEmailNickname] = {
    selectWhereEmail(email)
      .map(_.toIdEmailNickname)
      .orElse(insert(email).map {
        case (id, nickname) => IdEmailNickname(id, email, nickname)
      })
  }
}
