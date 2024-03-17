package models.tables

import anorm._
import anorm.SqlParser._
import play.api.Logging

import java.sql.Connection
import java.util.Date

case class IpDeny(
  seq: Long,
  ip: String,
  dateInserted: Date,
)

object IpDeny extends Logging {
  def tupled: ((Long, String, Date)) => IpDeny = (apply _).tupled

  def insert(ip: String, accessLog: Option[Long], reason: String)(implicit connection: Connection): Option[Long] = {
    SQL"""INSERT INTO IpDeny (ip, accessLog, reason) VALUES ($ip, $accessLog, $reason)""".executeInsert()
  }

  def selectCount(ip: String)(implicit connection: Connection): Long = {
    SQL"""SELECT COUNT(*) cnt FROM IpDeny WHERE ip = $ip""".as(long("cnt") single)
  }

  def selectLatest(ip: String)(implicit connection: Connection): Option[IpDeny] = {
    SQL"""SELECT seq, ip, dateInserted FROM IpDeny WHERE ip = $ip ORDER BY seq DESC LIMIT 1"""
      .as(long("seq") ~ str("ip") ~ date("dateInserted") singleOpt).map(flatten)
      .map(tupled)
  }
}
