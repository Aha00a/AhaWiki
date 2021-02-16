package models.tables

import java.sql.Connection

import anorm.SQL
import anorm.SqlParser.date
import anorm.SqlParser.flatten
import anorm.SqlParser.long
import anorm.SqlParser.str

import java.sql.Connection
import java.util.Date

import anorm._
import anorm.SqlParser.date
import anorm.SqlParser.flatten
import anorm.SqlParser.long
import anorm.SqlParser.str
import com.aha00a.commons.Implicits._
import com.aha00a.commons.utils.RangeUtil
import models.WithDateTime
import models.tables

import scala.collection.immutable
import scala.util.matching.Regex

case class Site(seq:Long, name:String) {
  
}

object Site {
  def tupled = (apply _).tupled

  def selectWhereDomain(domain: String)(implicit connection: Connection): Option[Site] = {
    SQL"""
        SELECT
            S.seq, S.name
            FROM Site S
            INNER JOIN SiteDomain SD ON S.seq = SD.site
            WHERE SD.domain = $domain
         """
      .as(long("seq") ~ str("name") singleOpt).map(flatten)
      .map(Site.tupled)
  }

  def selectRandom()(implicit connection: Connection): Option[Site] = {
    SQL"""
        SELECT
            S.seq, S.name
            FROM Site S
            ORDER BY RAND()
            LIMIT 1
         """
      .as(long("seq") ~ str("name") singleOpt).map(flatten)
      .map(Site.tupled)
  }
}
