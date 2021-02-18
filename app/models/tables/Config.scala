package models.tables

import java.util.Date

import anorm.SqlParser.date
import anorm.SqlParser.flatten
import anorm.SqlParser.str
import anorm._

case class Config(k: String, v: String, created: Date, updated: Date)

object Config {

  import java.sql.Connection

  //noinspection TypeAnnotation
  def tupled = (apply _).tupled

  def select(k: String)(implicit connection: Connection, site: Site): Option[Config] = {
    SQL"""
        SELECT k, v, created, updated
            FROM Config
            WHERE site = ${site.seq} AND k = $k"""
      .as(str("k") ~ str("v") ~ date("created") ~ date("updated") singleOpt).map(flatten)
      .map(tupled)
  }

  def getOrElse(k: String, default: String)(implicit connection: Connection, site: Site): String = select(k).map(_.v).getOrElse(default)

  // TODO: Strongly typed config

}

