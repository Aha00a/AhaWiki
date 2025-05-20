package models.tables

import anorm.SqlParser.date
import anorm.SqlParser.flatten
import anorm.SqlParser.long
import anorm.SqlParser.str
import anorm._

import java.sql.Connection
import java.util.Date

case class User(seq:Long, created: Date, updated: Date, email: String) {

}

object User {
  //noinspection TypeAnnotation
  def tupled = (apply _).tupled

  def selectWhereEmail(email: String)(implicit connection: Connection): Option[User] = {
    SQL"""
        SELECT
            U.seq, U.created, U.updated, U.email
            FROM User U
            WHERE U.email = $email
         """
      .as(long("seq") ~ date("created") ~ date("updated") ~ str("email") singleOpt).map(flatten)
      .map(User.tupled)
  }

  def insert(email: String)(implicit connection: Connection): Option[Long] = {
    SQL"""INSERT INTO User (email) VALUES ($email)""".executeInsert()
  }
}
