package services

import com.aha00a.commons.Implicits._
import com.aha00a.commons.utils.StopWatch
import logics.ApplicationConf
import models.LatLng
import models.tables.GeocodeCache
import play.api.Logging
import play.api.db.Database
import play.api.libs.json.Json
import play.api.libs.json.Reads
import play.api.libs.ws.WSClient

import javax.inject.Inject
import scala.concurrent.ExecutionContext

class ServiceGeocode @Inject()(
  database: Database,
  wsClient: WSClient,
  applicationConf: ApplicationConf,
)(implicit executionContext: ExecutionContext) extends Logging {
  private implicit val latLngReads: Reads[LatLng] = Json.reads[LatLng]

  def resolveAndCache(address: String): Unit = {
    StopWatch(s"Geocode\t$address") {
      if (address.isNotNullOrEmpty) {
        wsClient
          .url("https://maps.googleapis.com/maps/api/geocode/json")
          .withQueryStringParameters(
            "address" -> address,
            "key" -> applicationConf.AhaWiki.google.credentials.api.Geocoding.key(),
          )
          .get()
          .map { response =>
            logger.info(s"$address - ${response.json}")
            (response.json \ "results" \ 0 \ "geometry" \ "location").as[LatLng]
          }
          .map { latLng =>
            database.withConnection { implicit connection =>
              GeocodeCache.replace(address, latLng)
            }
          }
          .map(i => logger.info(i.toString))
          .recover {
            case t: Throwable =>
              logger.error(s"Geocode failed: $address", t)
          }
      }
    }
  }
}
