package models.tables

import anorm.SqlParser.{date, flatten, int, long, str}
import anorm._

import java.sql.Connection
import java.util.Date

case class UserSite(user: Long, site: Long, created: Date, email: String, nickname: String)

object UserSite {
  //noinspection TypeAnnotation
  def tupled = (apply _).tupled

  def select()(implicit connection: Connection, site: Site): Seq[UserSite] = {
    SQL"""
        SELECT
            US.user, US.site, US.created, U.email, U.nickname
            FROM UserSite US
            INNER JOIN User U ON US.user = U.seq
            WHERE US.site = ${site.seq}
            ORDER BY US.created DESC
         """
      .as(long("user") ~ long("site") ~ date("created") ~ str("email") ~ str("nickname") *).map(flatten)
      .map(UserSite.tupled)
  }
}
