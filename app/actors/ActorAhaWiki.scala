package actors

import akka.actor._
import com.aha00a.commons.Implicits._
import com.aha00a.commons.utils.StopWatch
import com.aha00a.stemmers.Stemmer
import javax.inject.Inject
import logics.wikis.interpreters.Interpreters
import logics.{AhaWikiCache, ApplicationConf}
import models.{AhaWikiQuery, LatLng, Link, SchemaOrg, WikiContext}
import play.api.Logging
import play.api.cache.SyncCacheApi
import play.api.cache.AsyncCacheApi
import play.api.db.Database
import play.api.libs.json.{Json, Reads}
import play.api.libs.ws.WSClient
import play.api.{Configuration, Logger}

import scala.concurrent.ExecutionContext

object ActorAhaWiki {
  def props: Props = Props[ActorAhaWiki]

  case class Calculate(name: String, i: Int = 1, length: Int = 1)

  case class CalculateCosineSimilarity(name: String, i: Int = 1, length: Int = 1)

  case class CalculateLink(name: String, i: Int = 1, length: Int = 1)

  case class Geocode(address: String)

  case class Distance(src: String, dst: String)

}

class ActorAhaWiki @Inject()(implicit syncCacheApi: SyncCacheApi, database: Database, ws: WSClient, executor: ExecutionContext, configuration: Configuration) extends Actor with Logging {

  import ActorAhaWiki._

  def receive: PartialFunction[Any, Unit] = {
    case Calculate(name: String, i: Int, length: Int) => StopWatch(s"$name\tCalculate($i/$length)") {
      context.self ! CalculateCosineSimilarity(name, i, length)
      context.self ! CalculateLink(name, i, length)
    }

    case CalculateCosineSimilarity(name: String, i: Int, length: Int) => StopWatch(s"$name\tCalculateCosineSimilarity($i/$length)") {
      database.withConnection { implicit connection =>
        val ahaWikiQuery = AhaWikiQuery()
        ahaWikiQuery.Page.selectLastRevision(name) foreach { page =>
          implicit val wikiContext: WikiContext = WikiContext(page.name)(null, syncCacheApi, database, context.self, configuration)
          val seq: Seq[String] = Interpreters.toSeqWord(page.content) // TODO
          logger.info(seq.mkString("(", ")\t(", ")"))

          val wordCount = Stemmer.removeStopWord(Stemmer.stem(page.content)).groupByCount()
          ahaWikiQuery.Page.updateSimilarPage(name, wordCount)
        }
      }
    }
    case CalculateLink(name: String, i: Int, length: Int) => StopWatch(s"$name\tCalculateLink($i/$length)") {
      database.withConnection { implicit connection =>
        val ahaWikiQuery = AhaWikiQuery()
        ahaWikiQuery.Page.selectLastRevision(name) foreach { page =>
          implicit val wikiContext: WikiContext = WikiContext(page.name)(null, syncCacheApi, database, context.self, configuration)
          val seqLink = Interpreters.toSeqLink(page.content).filterNot(_.isDstExternal) ++ Seq(Link(page.name, "", ""))
          ahaWikiQuery.Page.updateLink(page.name, seqLink)

          val seqSchemaOrg: Seq[SchemaOrg] = Interpreters.toSeqSchemaOrg(page.content)
          ahaWikiQuery.Page.updateSchemaOrg(name, seqSchemaOrg)
        }
      }
    }
    case Geocode(address) => StopWatch(s"Query Google Geocode - $address") {
      implicit val latLngReads: Reads[LatLng] = Json.reads[LatLng]
      ws
        .url("https://maps.googleapis.com/maps/api/geocode/json")
        .withQueryString(
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
            AhaWikiQuery().GeocodeCache.replace(address, latLng)
            AhaWikiCache.AddressToLatLng.set(address, latLng)
          }
        })
    }
    case Distance(src, dst) => StopWatch(s"Query Google Distance Matrix Api - $src - $dst") {
      ws
        .url("https://maps.googleapis.com/maps/api/distancematrix/json")
        .withQueryString(
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
            AhaWikiQuery().DistanceCache.replace(src, dst, metersSeconds._1, metersSeconds._2)
            AhaWikiCache.Distance.set(src, dst, metersSeconds._1)
          }
        })
    }
    case _ =>
      logger.error("Unknown")
  }

}

