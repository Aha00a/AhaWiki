package models.tables

import anorm.SqlParser.flatten
import anorm.SqlParser.long
import anorm.SqlParser.scalar
import anorm.SqlParser.str
import anorm._
import com.aha00a.play.AnormSqlParser.localDateTime

import java.sql.Connection
import java.time.LocalDate
import java.time.LocalDateTime

case class Habit(seq: Long, site: Int, user: Long, habitType: String, dateInserted: LocalDateTime)

object Habit {
  case class Entry(dateInserted: LocalDateTime, userSeq: Long, nickname: String)
  case class DailyCount(dateYmd: String, count: Int)

  private val rowParserEntry = localDateTime("dateInserted") ~ long("userSeq") ~ str("nickname")
  private val rowParserDailyCount = str("dateYmd") ~ SqlParser.int("count")

  def insert(habitType: String, userSeq: Long, dateInserted: LocalDateTime = LocalDateTime.now())(implicit connection: Connection, site: Site): Option[Long] = {
    SQL"""
      INSERT INTO Habit (site, user, type, dateInserted)
      VALUES (${site.seq}, $userSeq, $habitType, $dateInserted)
    """.executeInsert(scalar[Long].singleOpt)
  }

  def selectByType(habitType: String, limit: Int = 500)(implicit connection: Connection, site: Site): Seq[Entry] = {
    SQL"""
      SELECT H.dateInserted, H.user userSeq, U.nickname
        FROM Habit H
        INNER JOIN User U ON U.seq = H.user
        WHERE H.site = ${site.seq} AND H.type = $habitType
        ORDER BY H.dateInserted DESC
        LIMIT $limit
    """
      .as(rowParserEntry.*).map(flatten).map(Entry.tupled)
  }

  def countByType(habitType: String)(implicit connection: Connection, site: Site): Int = {
    SQL"""
      SELECT COUNT(*) AS count
        FROM Habit H
        WHERE H.site = ${site.seq} AND H.type = $habitType
    """.as(SqlParser.int("count").single)
  }

  def countByTypeSince(habitType: String, dateFrom: LocalDate)(implicit connection: Connection, site: Site): Int = {
    SQL"""
      SELECT COUNT(*) AS count
        FROM Habit H
        WHERE H.site = ${site.seq} AND H.type = $habitType AND H.dateYmd >= $dateFrom
    """.as(SqlParser.int("count").single)
  }

  def countDistinctUsersByTypeSince(habitType: String, dateFrom: LocalDate)(implicit connection: Connection, site: Site): Int = {
    SQL"""
      SELECT COUNT(DISTINCT H.user) AS count
        FROM Habit H
        WHERE H.site = ${site.seq} AND H.type = $habitType AND H.dateYmd >= $dateFrom
    """.as(SqlParser.int("count").single)
  }

  def countByTypeSinceForUser(habitType: String, userSeq: Long, dateFrom: LocalDate)(implicit connection: Connection, site: Site): Int = {
    SQL"""
      SELECT COUNT(*) AS count
        FROM Habit H
        WHERE H.site = ${site.seq} AND H.type = $habitType AND H.user = $userSeq AND H.dateYmd >= $dateFrom
    """.as(SqlParser.int("count").single)
  }

  def selectDailyCountByTypeSinceForUser(habitType: String, userSeq: Long, dateFrom: LocalDate, limit: Int = 30)(implicit connection: Connection, site: Site): Seq[DailyCount] = {
    SQL"""
      SELECT DATE_FORMAT(H.dateYmd, '%Y-%m-%d') AS dateYmd, COUNT(*) AS count
        FROM Habit H
        WHERE H.site = ${site.seq} AND H.type = $habitType AND H.user = $userSeq AND H.dateYmd >= $dateFrom
        GROUP BY H.dateYmd
        ORDER BY H.dateYmd DESC
        LIMIT $limit
    """.as(rowParserDailyCount.*).map(flatten).map(DailyCount.tupled)
  }
}
