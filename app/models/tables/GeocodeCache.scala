package models.tables

import anorm.SqlParser.double
import anorm.SqlParser.flatten
import anorm.SqlParser.str
import anorm._
import com.aha00a.play.AnormSqlParser.localDateTime
import models.LatLng
import java.time.LocalDateTime

case class GeocodeCache(address: String, lat: Double, lng: Double, created: LocalDateTime) {
  lazy val latLng: LatLng = LatLng(lat, lng)
}

object GeocodeCache {

  import java.sql.Connection

  //noinspection TypeAnnotation
  def tupled = (apply _).tupled

  // An empty Seq interpolates to `IN ()`, which is a syntax error rather than a query that finds
  // nothing, so the guard has to be here and not at each call site. It was missing here, and a
  // `#!Map` block whose body held no location answered 500 for the whole page.
  def select(seqAddress: Seq[String])(implicit connection: Connection): Seq[GeocodeCache] = {
    if (seqAddress.isEmpty) {
      Seq.empty
    } else {
      SQL"SELECT address, lat, lng, created FROM GeocodeCache WHERE address IN ($seqAddress)"
        .as(str("address") ~ double("lat") ~ double("lng") ~ localDateTime("created") *).map(flatten)
        .map(tupled)
    }
  }

  def replace(address: String, latLng: LatLng)(implicit connection: Connection): Int = {
    SQL"""REPLACE INTO GeocodeCache (address, lat, lng) VALUES ($address, ${latLng.lat}, ${latLng.lng})""".executeUpdate()
  }
}
