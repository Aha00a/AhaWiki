package actors

import akka.actor._
import com.twitter.penguin.korean.TwitterKoreanProcessor
import logics.wikis.Interpreters
import models.Database
import models.Database.Page
import utils.StopWatch
import implicits.Implicits._

object ActorPageProcessor {
  def props = Props[ActorPageProcessor]

  val regexNumber = """\d{1,3}""".r

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
    val wordCount = normalizeTokenizeStemFilter(page.content)
    Database.termFrequencyDelete(name)
    Database.termFrequencyInsert(name, wordCount)
    Database.cosineSimilarityUpdate(name)
  }

  def calcWordCount(s: String): Map[String, Int] = {
    s.replaceAll( """[^\w가-힣\s]+""", " ").split( """\s+""").filter {
      case "" | "wiki" | "http" | "com" | "www" | "the" | "to" | "of" | "for" | "is" | "a" | "in" | "and" | "be" | "or" => false
      case regexNumber() => false
      case string if 10 < string.length => false
      case _ => true
    }.toSeq.groupByCount()
  }
  def toCountMap(a: Seq[String]): Map[String, Int] = {
    a.foldLeft(Map[String, Int]()) {
      (map, word) => map + (word -> (map.getOrElse(word, 0) + 1))
    }
  }

  def updateLink(page:Page) = {
    val seqLink = Interpreters.extractLink(page.name, page.content)
    Database.linkDelete(page.name)
    Database.linkInsert(seqLink)
  }

  def normalizeTokenizeStemFilter(text: String): Map[String, Int] = {
    val nts = TwitterKoreanProcessor.stem(TwitterKoreanProcessor.tokenize(TwitterKoreanProcessor.normalize(text)))
    import com.twitter.penguin.korean.util.KoreanPos._
    val ntfs = nts.filter(a => Seq(Noun, Adjective, Verb).contains(a.pos)).map(_.text)
    ntfs.groupByCount()
  }

}


