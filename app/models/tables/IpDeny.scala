package models.tables

import anorm._
import anorm.SqlParser.{flatten, long, str}
import com.aha00a.play.AnormSqlParser.localDateTime
import play.api.Logging

import java.sql.Connection
import java.time.LocalDateTime

case class IpDeny(
  seq: Long,
  ip: String,
  dateInserted: LocalDateTime,
)

object IpDeny extends Logging {
  def tupled: ((Long, String, LocalDateTime)) => IpDeny = (apply _).tupled

  def insert(ip: String, accessLog: Option[Long], reason: String)(implicit connection: Connection): Option[Long] = {
    SQL"""INSERT INTO IpDeny (ip, accessLog, reason) VALUES ($ip, $accessLog, $reason)""".executeInsert()
  }

  def selectCount(ip: String)(implicit connection: Connection): Long = {
    SQL"""SELECT COUNT(*) cnt FROM IpDeny WHERE ip = $ip""".as(long("cnt") single)
  }

  def selectLatest(ip: String)(implicit connection: Connection): Option[IpDeny] = {
    SQL"""SELECT seq, ip, dateInserted FROM IpDeny WHERE ip = $ip ORDER BY seq DESC LIMIT 1"""
      .as(long("seq") ~ str("ip") ~ localDateTime("dateInserted") singleOpt).map(flatten)
      .map(tupled)
  }

  def deleteExpired(limit: Int = 1000)(implicit connection: Connection): Int = {
    SQL"""
        DELETE FROM IpDeny
            WHERE seq < (
                SELECT MAX(seq)
                    FROM (
                    SELECT seq, dateInserted
                        FROM IpDeny
                        ORDER BY seq
                        LIMIT $limit
                ) T
                WHERE T.dateInserted < DATE_ADD(NOW(), INTERVAL -5 YEAR)
            );
    """.executeUpdate()
  }
}
