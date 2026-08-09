package models.tables

import anorm._

import java.sql.Connection
import java.time.LocalDateTime
import java.time.Period
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

  // 관리자 화면이 사용자별 조회 이력을 보여주는 기간이다. IpDeny 의 90일과 맞춰져 있다.
  val Retention: Period = Period.ofMonths(3)

  def deleteExpired(limit: Int = 10000, now: LocalDateTime = LocalDateTime.now())(implicit connection: Connection): Int =
    ExpiredRows.deleteInsertedBefore("UserViewHistory", now.minus(Retention), limit)
}
