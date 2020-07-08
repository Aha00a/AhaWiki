package models.tables

import java.util.Date

import anorm.SqlParser.date
import anorm.SqlParser.flatten
import anorm.SqlParser.int
import anorm.SqlParser.str
import anorm._

case class DistanceCache(src: String, dst: String, meters: Int, seconds: Int, created: Date)

object DistanceCache {

  import java.sql.Connection

  //noinspection TypeAnnotation
  def tupled = (apply _).tupled

  def select(src: String, dst: String)(implicit connection: Connection): Option[DistanceCache] = {
    SQL"SELECT src, dst, meters, seconds, created FROM DistanceCache WHERE src = $src AND dst = $dst"
      .as(str("src") ~ str("dst") ~ int("meters") ~ int("seconds") ~ date("created") singleOpt).map(flatten)
      .map(DistanceCache.tupled)
  }

  def replace(src: String, dst: String, meters: Int, seconds: Int)(implicit connection: Connection): Int = {
    SQL"""REPLACE INTO DistanceCache (src, dst, meters, seconds) VALUES ($src, $dst, $meters, $seconds)""".executeUpdate()
  }
}

