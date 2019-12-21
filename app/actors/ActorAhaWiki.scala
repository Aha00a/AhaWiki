package actors

import akka.actor._
import com.aha00a.commons.Implicits._
import com.aha00a.commons.utils.StopWatch
import com.aha00a.stemmers.Stemmer
import javax.inject.Inject
import logics.wikis.Interpreters
import logics.{AhaWikiCache, ApplicationConf}
import models.Page
import models.{AhaWikiDatabase, LatLng, WikiContext}
import play.api.cache.CacheApi
import play.api.db.Database
import play.api.libs.json.{Json, Reads}
import play.api.libs.ws.WSClient
import play.api.{Configuration, Logger}

import scala.concurrent.ExecutionContext

object ActorAhaWiki {
  def props: Props = Props[ActorAhaWiki]

  case class Calculate(name: String, i: Int = 1, length: Int = 1)
  case class Geocode(address: String)
}

class ActorAhaWiki @Inject()(implicit cacheApi: CacheApi, db: Database, ws: WSClient, executor: ExecutionContext, configuration: Configuration) extends Actor {
  import ActorAhaWiki._
  private val ahaWikiDatabase = AhaWikiDatabase()

  def receive: PartialFunction[Any, Unit] = {
    case Calculate(name: String, i: Int, length: Int) => StopWatch(s"$name - ($i/$length)") {
      ahaWikiDatabase.Page.selectLastRevision(name) foreach { page =>
        updateCosineSimilarity(name, page)
        updateLink(page)
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
          Logger.info(s"$address - ${r.json}")
          (r.json \ "results" \ 0 \ "geometry" \ "location").as[LatLng]
        })
        .map(latLng => {
          ahaWikiDatabase.GeocodeCache.replace(address, latLng)
          AhaWikiCache.AddressToLatLng.set(address, latLng)
        })
    }
    case _ =>
      Logger.error("Unknown")
  }

  def updateCosineSimilarity(name: String, page: Page): Unit = {
    val wordCount = Stemmer.removeStopWord(Stemmer.stem(page.content)).groupByCount()
    ahaWikiDatabase.TermFrequency.delete(name)
    ahaWikiDatabase.TermFrequency.insert(name, wordCount)
    ahaWikiDatabase.CosineSimilarity.recalc(name)
  }

  def updateLink(page: Page): Array[Int] = {
    implicit val wikiContext: WikiContext = WikiContext(page.name)(null, cacheApi, db, context.self, configuration)
    val seqLink = Interpreters.extractLink(page.name, page.content)
    ahaWikiDatabase.Link.delete(page.name)
    ahaWikiDatabase.Link.insert(seqLink)
  }
}

