package models.tables

import anorm.SqlParser._
import anorm._
import com.aha00a.play.AnormSqlParser.localDateTime

import java.sql.Connection
import java.time.LocalDateTime

case class UserEmail(user: Long, email: String, isPrimary: Boolean, created: LocalDateTime)

object UserEmail {
  //noinspection TypeAnnotation
  def tupled = (apply _).tupled

  private val rowParser = long("user") ~ str("email") ~ bool("isPrimary") ~ localDateTime("created")

  def selectByEmail(email: String)(implicit connection: Connection): Option[UserEmail] = {
    SQL"""
      SELECT user, email, isPrimary, created
      FROM UserEmail
      WHERE email = $email
    """.as(rowParser.singleOpt).map(flatten).map(UserEmail.tupled)
  }

  def selectByUser(user: Long)(implicit connection: Connection): Seq[UserEmail] = {
    SQL"""
      SELECT user, email, isPrimary, created
      FROM UserEmail
      WHERE user = $user
      ORDER BY isPrimary DESC, email
    """.as(rowParser.*).map(flatten).map(UserEmail.tupled)
  }

  def selectEmailsByUser(user: Long)(implicit connection: Connection): Seq[String] =
    selectByUser(user).map(_.email)

  def selectPrimaryEmailByUser(user: Long)(implicit connection: Connection): Option[String] =
    selectByUser(user).find(_.isPrimary).orElse(selectByUser(user).headOption).map(_.email)

  def insert(user: Long, email: String, isPrimary: Boolean)(implicit connection: Connection): Int = {
    SQL"""
      INSERT INTO UserEmail (user, email, isPrimary)
      VALUES ($user, $email, $isPrimary)
    """.executeUpdate()
  }

  def delete(user: Long, email: String)(implicit connection: Connection): Int = {
    SQL"""
      DELETE FROM UserEmail
      WHERE user = $user
        AND email = $email
    """.executeUpdate()
  }

  def setPrimary(user: Long, email: String)(implicit connection: Connection): Int = {
    SQL"UPDATE UserEmail SET isPrimary = false WHERE user = $user".executeUpdate()
    SQL"""
      UPDATE UserEmail
      SET isPrimary = true
      WHERE user = $user
        AND email = $email
    """.executeUpdate()
  }

  def hasPrimary(user: Long)(implicit connection: Connection): Boolean =
    SQL"""
      SELECT COUNT(*) AS cnt
      FROM UserEmail
      WHERE user = $user
        AND isPrimary = true
    """.as(long("cnt").single) > 0
}
