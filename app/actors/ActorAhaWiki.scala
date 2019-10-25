package actors

import javax.inject.Inject
import akka.actor._
import com.aha00a.commons.implicits.Implicits._
import com.aha00a.commons.utils.{Stemmer, StopWatch}
import logics.wikis.Interpreters
import models.AhaWikiDatabase.Page
import models.{AhaWikiDatabase, WikiContext}
import play.api.cache.CacheApi
import play.api.db.Database

import scala.concurrent.duration.Duration
import scala.reflect.ClassTag

object ActorAhaWiki {
  def props: Props = Props[ActorAhaWiki]


  case class Calculate(name: String, i:Int = 1, length: Int = 1)
}

class ActorAhaWiki @Inject()(implicit cacheApi: CacheApi, db: Database) extends Actor {
  import ActorAhaWiki._

  def receive: PartialFunction[Any, Unit] = {
    case Calculate(name: String, i:Int, length:Int) =>
      StopWatch(s"$name - ($i/$length)") {
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
    implicit val wikiContext: WikiContext = WikiContext(page.name)(null, cacheApi, db)
    
    val seqLink = Interpreters.extractLink(page.name, page.content)
    AhaWikiDatabase().linkDelete(page.name)
    AhaWikiDatabase().linkInsert(seqLink)
  }


}


