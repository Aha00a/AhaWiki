package models.tables

import java.util.Date

import anorm._
import anorm.SqlParser.date
import anorm.SqlParser.double
import anorm.SqlParser.flatten
import anorm.SqlParser.str
import models.LatLng

case class GeocodeCache(address: String, lat: Double, lng: Double, created: Date) {
  lazy val latLng: LatLng = LatLng(lat, lng)
}

//noinspection TypeAnnotation
object GeocodeCache {

  import java.sql.Connection

  def tupled = (apply _).tupled

  private val rowParser = str("address") ~ double("lat") ~ double("lng") ~ date("created")

  def select(address: String)(implicit connection: Connection): Option[GeocodeCache] = {
    import models.tables.GeocodeCache
    SQL"SELECT address, lat, lng, created FROM GeocodeCache WHERE address = $address"
      .as(rowParser singleOpt).map(flatten)
      .map(tupled)
  }

  def replace(address: String, latLng: LatLng)(implicit connection: Connection): Int = {
    SQL"""REPLACE INTO GeocodeCache (address, lat, lng) VALUES ($address, ${latLng.lat}, ${latLng.lng})""".executeUpdate()
  }
}

