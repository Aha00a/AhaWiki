package actors

import akka.actor._
import com.aha00a.commons.implicits.Implicits
import logics.wikis.Interpreters
import models.{WikiContext, Database}
import models.Database.Page
import com.aha00a.commons.utils.{Stemmer, StopWatch}
import Implicits._
import play.api.cache.CacheApi

import scala.concurrent.duration.Duration

object ActorPageProcessor {
  def props = Props[ActorPageProcessor]


  case class Calculate(name: String)
}

class ActorPageProcessor extends Actor {
  import ActorPageProcessor._

  def receive = {
    case Calculate(name: String) =>
      StopWatch(name) {
        Database.pageSelectLastRevision(name) foreach { page =>
          updateCosineSimilarity(name, page)
          updateLink(page)
        }
      }
  }

  def updateCosineSimilarity(name: String, page: Page): Unit = {
    val wordCount = Stemmer.removeStopWord(Stemmer.stem(page.content)).groupByCount()
    Database.termFrequencyDelete(name)
    Database.termFrequencyInsert(name, wordCount)
    Database.cosineSimilarityUpdate(name)
  }


  def updateLink(page:Page) = {
    // TODO: inject CacheApi
    implicit val wikiContext: WikiContext = WikiContext(page.name)(null, new CacheApi {
      override def set(key: String, value: Any, expiration: Duration): Unit = {}

      override def get[T](key: String)(implicit evidence$2: ClassManifest[T]): Option[T] = None

      override def getOrElse[A](key: String, expiration: Duration)(orElse: => A)(implicit evidence$1: ClassManifest[A]): A = orElse

      override def remove(key: String): Unit = {}
    })

    val seqLink = Interpreters.extractLink(page.name, page.content)
    Database.linkDelete(page.name)
    Database.linkInsert(seqLink)
  }


}


