package actors

import javax.inject.Inject

import akka.actor._
import com.aha00a.commons.implicits.Implicits
import logics.wikis.Interpreters
import models.{AhaWikiDatabase, WikiContext}
import models.AhaWikiDatabase.Page
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
        AhaWikiDatabase().pageSelectLastRevision(name) foreach { page =>
          updateCosineSimilarity(name, page)
          updateLink(page)
        }
      }
  }

  def updateCosineSimilarity(name: String, page: Page): Unit = {
    val wordCount = Stemmer.removeStopWord(Stemmer.stem(page.content)).groupByCount()
    AhaWikiDatabase().termFrequencyDelete(name)
    AhaWikiDatabase().termFrequencyInsert(name, wordCount)
    AhaWikiDatabase().cosineSimilarityUpdate(name)
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
    AhaWikiDatabase().linkDelete(page.name)
    AhaWikiDatabase().linkInsert(seqLink)
  }


}


