package models.tables

import java.util.Date

import anorm.SqlParser.date
import anorm.SqlParser.double
import anorm.SqlParser.flatten
import anorm.SqlParser.str
import anorm._
import models.LatLng

case class GeocodeCache(address: String, lat: Double, lng: Double, created: Date) {
  lazy val latLng: LatLng = LatLng(lat, lng)
}

object GeocodeCache {

  import java.sql.Connection

  //noinspection TypeAnnotation
  def tupled = (apply _).tupled

  def select(seqAddress: Seq[String])(implicit connection: Connection): Seq[GeocodeCache] = {
    SQL"SELECT address, lat, lng, created FROM GeocodeCache WHERE address IN ($seqAddress)"
      .as(str("address") ~ double("lat") ~ double("lng") ~ date("created") *).map(flatten)
      .map(tupled)
  }

  def replace(address: String, latLng: LatLng)(implicit connection: Connection): Int = {
    SQL"""REPLACE INTO GeocodeCache (address, lat, lng) VALUES ($address, ${latLng.lat}, ${latLng.lng})""".executeUpdate()
  }
}

