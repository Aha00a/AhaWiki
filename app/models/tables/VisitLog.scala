package models.tables

import java.util.Date

import anorm.SqlParser.date
import anorm.SqlParser.double
import anorm.SqlParser.flatten
import anorm.SqlParser.str
import anorm._
import models.LatLng

case class VisitLog(seq: Long, site: Long, name: String, created: Date, remoteAddress: String, userAgent: String, referer: String, refererSite: Option[Long], refererName: String) {
}

object VisitLog {

  import java.sql.Connection

  //noinspection TypeAnnotation
  def tupled = (apply _).tupled

  def insert(site: Long, name: String, remoteAddress: String, userAgent: String, referer: String, refererSite: Option[Long], refererName: String)(implicit connection: Connection): Int = {
    SQL"""
      INSERT INTO VisitLog (site, name, remoteAddress, userAgent, referer, refererSite, refererName)
      VALUES ($site, $name, $remoteAddress, $userAgent, $referer, $refererSite, $refererName)
    """.executeUpdate()
  }
}


