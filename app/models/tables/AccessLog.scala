package models.tables

import anorm._
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
    uri: String, // TODO: rename to path
    remoteAddress: String,
    userAgent: String,
    status: Int,
    durationMilli: Int,
  )(implicit connection: Connection): Option[Long] = {
    SQL"""
        INSERT INTO AccessLog
                (site, user, ipDeny, method, scheme, host, uri, remoteAddress, userAgent, status, durationMilli)
            VALUES
                ($site, $user, $ipDeny, $method, $scheme, $host, $uri, $remoteAddress, $userAgent, $status, $durationMilli)
    """.executeInsert()
  }

  def deleteExpired(limit: Int = 10000)(implicit connection: Connection): Int = {
    // language=SQL
    SQL"""
        DELETE FROM AccessLog
            WHERE seq < (
                SELECT MAX(seq)
                    FROM (
                    SELECT seq, dateInserted
                        FROM AccessLog
                        ORDER BY seq
                        LIMIT $limit
                ) T
                WHERE T.dateInserted < DATE_ADD(NOW(), INTERVAL -1 MONTH)
            );
    """.executeUpdate()
  }
}
