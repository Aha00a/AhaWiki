package actors

import akka.actor._
import com.aha00a.commons.Implicits._
import com.aha00a.commons.utils.StopWatch
import com.aha00a.stemmers.Stemmer
import javax.inject.Inject
import logics.wikis.Interpreters
import logics.{AhaWikiCache, ApplicationConf}
import models.AhaWikiDatabase.Page
import models.{AhaWikiDatabase, LatLng, WikiContext}
import play.api.cache.CacheApi
import play.api.db.Database
import play.api.libs.json.{Json, Reads}
import play.api.libs.ws.WSClient
import play.api.{Configuration, Logger}

import scala.concurrent.ExecutionContext

object ActorAhaWiki {
  def props: Props = Props[ActorAhaWiki]

  case class Calculate(name: String, i:Int = 1, length: Int = 1)
  case class Geocode(address: String)
}

class ActorAhaWiki @Inject()(implicit cacheApi: CacheApi, db: Database, ws: WSClient, executor: ExecutionContext, configuration: Configuration) extends Actor {
  import ActorAhaWiki._

  def receive: PartialFunction[Any, Unit] = {
    case Calculate(name: String, i:Int, length:Int) =>
      StopWatch(s"$name - ($i/$length)") {
        AhaWikiDatabase().pageSelectLastRevision(name) foreach { page =>
          updateCosineSimilarity(name, page)
          updateLink(page)
        }
      }
    case Geocode(address) =>
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
          AhaWikiCache.AddressToLatLng.set(address, latLng)
          Logger.info(s"$address - $latLng")
        })
    case _ =>
      Logger.error("Unknown")
  }

  def updateCosineSimilarity(name: String, page: Page): Unit = {
    val wordCount = Stemmer.removeStopWord(Stemmer.stem(page.content)).groupByCount()
    AhaWikiDatabase().termFrequencyDelete(name)
    AhaWikiDatabase().termFrequencyInsert(name, wordCount)
    AhaWikiDatabase().cosineSimilarityUpdate(name)
  }

  def updateLink(page:Page): Array[Int] = {
    implicit val wikiContext: WikiContext = WikiContext(page.name)(null, cacheApi, db, context.self, configuration)
    val seqLink = Interpreters.extractLink(page.name, page.content)
    AhaWikiDatabase().linkDelete(page.name)
    AhaWikiDatabase().linkInsert(seqLink)
  }


}


