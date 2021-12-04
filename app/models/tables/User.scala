package models.tables

import java.sql.Connection

import anorm.SQL
import anorm.SqlParser.date
import anorm.SqlParser.flatten
import anorm.SqlParser.long
import anorm.SqlParser.str

import java.sql.Connection
import java.util.Date

import anorm._
import anorm.SqlParser.date
import anorm.SqlParser.flatten
import anorm.SqlParser.long
import anorm.SqlParser.str
import com.aha00a.commons.Implicits._
import com.aha00a.commons.utils.RangeUtil
import models.WithDateTime
import models.tables

import scala.collection.immutable
import scala.util.matching.Regex

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
