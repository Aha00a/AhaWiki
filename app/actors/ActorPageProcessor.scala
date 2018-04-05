package actors

import javax.inject.Inject

import akka.actor._
import com.aha00a.commons.implicits.Implicits
import logics.wikis.Interpreters
import models.{Database, WikiContext}
import models.Database.Page
import com.aha00a.commons.utils.{Stemmer, StopWatch}
import Implicits._
import play.api.cache.CacheApi
import play.api.db.{DB, DBApi, Database}

import scala.concurrent.duration.Duration
import scala.reflect.ClassTag

object ActorPageProcessor {
  def props: Props = Props[ActorPageProcessor]


  case class Calculate(name: String)
}

class ActorPageProcessor @Inject() extends Actor {
  import ActorPageProcessor._

  def receive: PartialFunction[Any, Unit] = {
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

  def updateLink(page:Page): Array[Int] = {
    // TODO: inject CacheApi
    implicit val wikiContext: WikiContext = WikiContext(page.name)(null, new CacheApi {
      override def set(key: String, value: Any, expiration: Duration): Unit = {}

      override def get[T: ClassTag](key: String): Option[T] = None

      override def getOrElse[A:ClassTag](key: String, expiration: Duration)(orElse: => A): A = orElse

      override def remove(key: String): Unit = {}
    })

    val seqLink = Interpreters.extractLink(page.name, page.content)
    Database.linkDelete(page.name)
    Database.linkInsert(seqLink)
  }


}


