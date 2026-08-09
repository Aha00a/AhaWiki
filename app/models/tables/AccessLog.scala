package models.tables

import anorm._
import play.api.Logging

import java.sql.Connection
import java.time.LocalDateTime
import java.time.Period
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
    path: String,
    remoteAddress: String,
    userAgent: String,
    status: Int,
    durationMilli: Int,
  )(implicit connection: Connection): Option[Long] = {
    SQL"""
        INSERT INTO AccessLog
                (site, user, ipDeny, method, scheme, host, uri, remoteAddress, userAgent, status, durationMilli)
            VALUES
                ($site, $user, $ipDeny, $method, $scheme, $host, $path, $remoteAddress, $userAgent, $status, $durationMilli)
    """.executeInsert()
  }

  // 180일 → 1년 → 6개월 → 3개월 → 1개월 로 계속 조여왔다. 이 테이블이 가장 빨리 자란다.
  val Retention: Period = Period.ofMonths(1)

  def deleteExpired(limit: Int = 10000, now: LocalDateTime = LocalDateTime.now())(implicit connection: Connection): Int =
    ExpiredRows.deleteInsertedBefore("AccessLog", now.minus(Retention), limit)
}
