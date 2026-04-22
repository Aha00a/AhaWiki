package models.tables

import anorm.SqlParser.{flatten, long, str}
import anorm._
import com.aha00a.play.AnormSqlParser.localDateTime

import java.sql.Connection
import java.time.LocalDateTime

case class UserSite(user: Long, site: Long, created: LocalDateTime, email: String, nickname: String)

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
      .as(localDateTime("created") ~ long("user") ~ long("site") ~ str("email") ~ str("nickname") *).map(flatten)
        .map { case (created, user, site, email, nickname) => (user, site, created, email, nickname) }
        .map(UserSite.tupled)
  }
}
