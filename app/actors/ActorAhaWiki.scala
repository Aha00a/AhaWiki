package actors

import actors.ActorAhaWiki._
import akka.actor._
import com.aha00a.commons.Implicits._
import com.aha00a.commons.utils.StopWatch
import logics.ApplicationConf
import logics.wikis.PageLogic
import models.ContextSite.RequestWrapper
import models.LatLng
import models.tables.GeocodeCache
import models.tables.Site
import play.api.Configuration
import play.api.Logger
import play.api.Logging
import play.api.db.Database
import play.api.libs.json.Json
import play.api.libs.json.Reads
import play.api.libs.ws.WSClient

import javax.inject.Inject
import scala.concurrent.ExecutionContext

object ActorAhaWiki {

  def props: Props = Props[ActorAhaWiki]

  case class Calculate(site: Site, name: String, i: Int = 1, length: Int = 1)

  case class Geocode(address: String)

}

class ActorAhaWiki @Inject()(implicit
                             database: Database,
                             wsClient: WSClient,
                             executionContext: ExecutionContext,
                             configuration: Configuration,
                            ) extends Actor with Logging {


  implicit val provider: RequestWrapper = RequestWrapper.empty


  //noinspection ScalaUnusedSymbol
  def receive: PartialFunction[Any, Unit] = {
    case c@Calculate(site: Site, name: String, i: Int, length: Int) =>
      StopWatch(c.toString) {
        database.withConnection { implicit connection =>
          implicit val implicitActorRef: ActorAhaWiki = this;
          implicit val implicitLogger: Logger = logger
          implicit val implicitSite: Site = site
          PageLogic.calculate(name)
        }
      }

    case g@Geocode(address) =>
      StopWatch(g.toString) {
        if (address.isNotNullOrEmpty) {
          implicit val latLngReads: Reads[LatLng] = Json.reads[LatLng]
          wsClient
            .url("https://maps.googleapis.com/maps/api/geocode/json")
            .withQueryStringParameters(
              "address" -> address,
              "key" -> ApplicationConf().AhaWiki.google.credentials.api.Geocoding.key()
            )
            .get()
            .map(r => {
              logger.info(s"$address - ${r.json}")
              (r.json \ "results" \ 0 \ "geometry" \ "location").as[LatLng]
            })
            .map(latLng => {
              database.withConnection { implicit connection =>
                GeocodeCache.replace(address, latLng)
              }
            })
            .map(i => logger.info(i.toString))
        }
      }
    case _ =>
      logger.error("Unknown")
  }

}

