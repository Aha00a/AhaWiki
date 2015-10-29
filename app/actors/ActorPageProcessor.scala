package actors

import akka.actor._
import logics.wikis.Interpreters
import models.Database
import models.Database.Page
import utils.{Stemmer, StopWatch}
import implicits.Implicits._

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
    val seqLink = Interpreters.extractLink(page.name, page.content)
    Database.linkDelete(page.name)
    Database.linkInsert(seqLink)
  }


}


