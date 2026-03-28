package models.tables

import anorm._

import java.sql.Connection
import java.util.Date

case class UserViewHistory(
  seq: Long,
  user: Long,
  site: Long,
  pageName: String,
  dateInserted: Date,
)

object UserViewHistory {
  def insert(user: Long, site: Long, pageName: String)(implicit connection: Connection): Option[Long] = {
    SQL"""
      INSERT INTO UserViewHistory
        (user, site, pageName)
      VALUES
        ($user, $site, $pageName)
    """.executeInsert()
  }

  def deleteExpired(limit: Int = 10000)(implicit connection: Connection): Int = {
    SQL"""
      DELETE FROM UserViewHistory
      WHERE seq < (
        SELECT MAX(seq)
        FROM (
          SELECT seq, dateInserted
          FROM UserViewHistory
          ORDER BY seq
          LIMIT $limit
        ) T
        WHERE T.dateInserted < DATE_ADD(NOW(), INTERVAL -3 MONTH)
      )
    """.executeUpdate()
  }
}
