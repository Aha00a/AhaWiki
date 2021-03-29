package models.tables

import java.util.Date

import anorm.SqlParser.date
import anorm.SqlParser.double
import anorm.SqlParser.flatten
import anorm.SqlParser.str
import anorm._
import models.LatLng

case class VisitLog(seq: Long, site: Long, name: String, created: Date, referer: String, remoteAddress: String) {
}

object VisitLog {

  import java.sql.Connection

  //noinspection TypeAnnotation
  def tupled = (apply _).tupled

  def insert(site: Long, name: String, remoteAddress: String, referer: String, refererSite: Option[Long], refererName: String)(implicit connection: Connection): Int = {
    SQL"""
      INSERT INTO VisitLog (site, name, remoteAddress, referer, refererSite, refererName)
      VALUES ($site, $name, $remoteAddress, $referer, $refererSite, $refererName)
    """.executeUpdate()
  }
}


