package models.tables

import java.util.Date

import anorm.SqlParser.date
import anorm.SqlParser.double
import anorm.SqlParser.flatten
import anorm.SqlParser.str
import anorm._
import models.LatLng

case class Referer(seq: Long, site: Long, name: String, created: Date, url: String, remoteAddress: String) {
}

object Referer {

  import java.sql.Connection

  //noinspection TypeAnnotation
  def tupled = (apply _).tupled

  def insert(site: Long, name: String, url: String, remoteAddress: String)(implicit connection: Connection): Int = {
    SQL"""INSERT INTO Referer (site, name, url, remoteAddress) VALUES ($site, $name, $url, $remoteAddress)""".executeUpdate()
  }
}




