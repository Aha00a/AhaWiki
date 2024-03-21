package models.tables

import anorm.SqlParser.date
import anorm.SqlParser.flatten
import anorm.SqlParser.str
import anorm._
import com.aha00a.commons.Implicits._
import play.api.Logging

import java.sql.Connection
import java.util.Date

case class AccessLog(
  id: Long,
  site: Long,
  user: Option[Long],
  dateInserted: Date,
  method: String,
  path: String,
  remoteAddress: String,
  userAgent: String,
  status: Int,
  durationMilli: Int,
)

object AccessLog extends Logging {
  def insert(
    site: Long,
    user: Option[Long],
    ipDeny: Option[Long],
    method: String,
    scheme: String,
    host: String,
    uri: String,
    url: String,
    remoteAddress: String,
    userAgent: String,
    status: Int,
    durationMilli: Int,
  )(implicit connection: Connection): Option[Long] = {
    SQL"""
        INSERT INTO AccessLog
                (site, user, ipDeny, method, scheme, host, uri, url, remoteAddress, userAgent, status, durationMilli)
            VALUES
                ($site, $user, $ipDeny, $method, $scheme, $host, $uri, $url, $remoteAddress, $userAgent, $status, $durationMilli)
    """.executeInsert()
  }

  def deleteExpired(limit: Int = 1000)(implicit connection: Connection): Int = {
    SQL"""DELETE FROM AccessLog WHERE dateInserted < DATE_ADD(NOW(), INTERVAL -180 DAY) LIMIT $limit""".executeUpdate()
  }
}
