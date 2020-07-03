package models.tables

import java.util.Date

import anorm.SqlParser.date
import anorm.SqlParser.flatten
import anorm.SqlParser.int
import anorm.SqlParser.str
import anorm._

case class DistanceCache(src: String, dst: String, meters: Int, seconds: Int, created: Date)

//noinspection TypeAnnotation
object DistanceCache {

  import java.sql.Connection

  def tupled = (apply _).tupled

  val rowParser = str("src") ~ str("dst") ~ int("meters") ~ int("seconds") ~ date("created")

  def select(src: String, dst: String)(implicit connection: Connection): Option[DistanceCache] = {
    SQL"SELECT src, dst, meters, seconds, created FROM DistanceCache WHERE src = $src AND dst = $dst"
      .as(rowParser singleOpt).map(flatten)
      .map(DistanceCache.tupled)
  }

  def replace(src: String, dst: String, meters: Int, seconds: Int)(implicit connection: Connection): Int = {
    SQL"""REPLACE INTO DistanceCache (src, dst, meters, seconds) VALUES ($src, $dst, $meters, $seconds)""".executeUpdate()
  }
}

