package models.tables

import anorm.SqlParser.flatten
import anorm.SqlParser.long
import anorm.SqlParser.scalar
import anorm.SqlParser.str
import anorm._
import com.aha00a.play.AnormSqlParser.localDateTime

import java.sql.Connection
import java.time.LocalDateTime

case class Habit(seq: Long, site: Int, user: Long, habitType: String, dateInserted: LocalDateTime)

object Habit {
  case class Entry(dateInserted: LocalDateTime, userSeq: Long, nickname: String)

  private val rowParserEntry = localDateTime("dateInserted") ~ long("userSeq") ~ str("nickname")

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
}
