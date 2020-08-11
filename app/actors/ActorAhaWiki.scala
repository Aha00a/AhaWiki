package actors

import akka.actor._
import com.aha00a.commons.Implicits._
import com.aha00a.commons.utils.StopWatch
import com.aha00a.stemmers.Stemmer
import javax.inject.Inject
import logics.wikis.interpreters.Interpreters
import logics.AhaWikiCache
import logics.ApplicationConf
import models.tables.Page
import models.LatLng
import models.WikiContext
import play.api.Configuration
import play.api.Logging
import play.api.cache.SyncCacheApi
import play.api.db.Database
import play.api.libs.json.Json
import play.api.libs.json.Reads
import play.api.libs.ws.WSClient

import scala.concurrent.ExecutionContext

object ActorAhaWiki {
  def props: Props = Props[ActorAhaWiki]

  case class Calculate(name: String, i: Int = 1, length: Int = 1)

  case class CalculateCosineSimilarity(name: String, i: Int = 1, length: Int = 1)

  case class CalculateLink(name: String, i: Int = 1, length: Int = 1)

  case class Geocode(address: String)

  case class Distance(src: String, dst: String)

}

class ActorAhaWiki @Inject()(implicit
                             syncCacheApi: SyncCacheApi,
                             database: Database,
                             wsClient: WSClient,
                             executionContext: ExecutionContext,
                             configuration: Configuration
                            ) extends Actor with Logging {

  import ActorAhaWiki._

  def receive: PartialFunction[Any, Unit] = {
    case Calculate(name: String, i: Int, length: Int) => StopWatch(s"$name\tCalculate($i/$length)") {
      context.self ! CalculateCosineSimilarity(name, i, length)
      context.self ! CalculateLink(name, i, length)
    }

    case CalculateCosineSimilarity(name: String, i: Int, length: Int) => StopWatch(s"$name\tCalculateCosineSimilarity($i/$length)") {
      database.withConnection { implicit connection =>
        Page.selectLastRevision(name) foreach { page =>
          import logics.AhaWikiInjects
          import logics.IdProvider
          import logics.wikis.RenderingMode

          implicit val ahaWikiInjects = AhaWikiInjects()
          implicit val wikiContext: WikiContext = new WikiContext(Seq(page.name), RenderingMode.Normal)(null, ahaWikiInjects, new IdProvider {
            override def getId: Option[String] = None
          })
          val seq: Seq[String] = Interpreters.toSeqWord(page.content) // TODO
          logger.info("toSeqWord")
          logger.info(seq.mkString("(", ")\t(", ")"))

          logger.info("toText")
          logger.info(Interpreters.toText(page.content))

          val wordCount = Stemmer.removeStopWord(Stemmer.stem(page.content)).groupByCount()
          Page.updateSimilarPage(name, wordCount)
        }
      }
    }
    case CalculateLink(name: String, i: Int, length: Int) => StopWatch(s"$name\tCalculateLink($i/$length)") {
      database.withConnection { implicit connection =>
        Page.selectLastRevision(name) foreach { page =>
          import logics.AhaWikiInjects
          import models.tables.Link
          import models.tables.SchemaOrg
          implicit val ahaWikiInjects = AhaWikiInjects()
          implicit val wikiContext: WikiContext = WikiContext(page.name)(null, ahaWikiInjects)
          val seqLink = Interpreters.toSeqLink(page.content).filterNot(_.isDstExternal) ++ Seq(Link(page.name, "", ""))
          Page.updateLink(page.name, seqLink)

          val seqSchemaOrg: Seq[SchemaOrg] = Interpreters.toSeqSchemaOrg(page.content)
          Page.updateSchemaOrg(name, seqSchemaOrg)
        }
      }
    }
    case Geocode(address) => StopWatch(s"Query Google Geocode - $address") {
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
          database.withTransaction { implicit connection =>
            import models.tables.GeocodeCache
            GeocodeCache.replace(address, latLng)
            AhaWikiCache.AddressToLatLng.set(address, latLng)
          }
        })
    }
    case Distance(src, dst) => StopWatch(s"Query Google Distance Matrix Api - $src - $dst") {
      wsClient
        .url("https://maps.googleapis.com/maps/api/distancematrix/json")
        .withQueryStringParameters(
          "mode" -> "transit",
          "origins" -> src,
          "destinations" -> dst,
          "key" -> ApplicationConf().AhaWiki.google.credentials.api.Geocoding.key()
        )
        .get()
        .map(r => {
          logger.info(s"$src - $dst - ${r.json}")
          (
            (r.json \ "rows" \ 0 \ "elements" \ 0 \ "distance" \ "value").as[Int],
            (r.json \ "rows" \ 0 \ "elements" \ 0 \ "duration" \ "value").as[Int]
          )
        })
        .map(metersSeconds => {
          database.withTransaction { implicit connection =>
            import models.tables.DistanceCache
            DistanceCache.replace(src, dst, metersSeconds._1, metersSeconds._2)
            AhaWikiCache.Distance.set(src, dst, metersSeconds._1)
          }
        })
    }
    case _ =>
      logger.error("Unknown")
  }

}

