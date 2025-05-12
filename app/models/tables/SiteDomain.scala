package models.tables

import anorm.SqlParser.date
import anorm.SqlParser.flatten
import anorm.SqlParser.long
import anorm.SqlParser.str
import anorm._

import java.sql.Connection
import java.util.Date

case class SiteDomain(created: Date, site: Long, domain: String)

object SiteDomain {
  val notFound: SiteDomain = SiteDomain(null, -1, "notFound")

  //noinspection TypeAnnotation
  def tupled = (apply _).tupled

  def select()(implicit connection: Connection): Seq[SiteDomain] = {
    SQL"""SELECT * FROM SiteDomain ORDER BY site"""
      .as(date("created") ~ long("site") ~ str("domain") *).map(flatten)
      .map(SiteDomain.tupled)
  }
}
