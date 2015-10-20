package actors

import java.time.{Duration, LocalDateTime}

import akka.actor._
import logics.wikis.Interpreters
import models.DirectQuery.Page
import models.{DirectQuery, PageContent}
import play.api.Logger


object ActorPageProcessor {
  def props = Props[ActorPageProcessor]

  val regexNumber = """\d{1,3}""".r

  case class Calculate(name: String)
}

class ActorPageProcessor extends Actor {
  import ActorPageProcessor._

  def receive = {
    case Calculate(name: String) =>
      val now = LocalDateTime.now()
      Logger.info(s"Start - $name")
      DirectQuery.pageSelectLastRevision(name).foreach(page => {
        updateCosineSimilarity(name, page)
        updateLink(page)
      })
      Logger.info(s"Done - $name - ${Duration.between(LocalDateTime.now(), now)}")
  }

  def updateCosineSimilarity(name: String, page: Page): Unit = {
    val wordCount = calcWordCount(page.content)
    DirectQuery.termFrequencyDelete(name)
    DirectQuery.termFrequencyInsert(name, wordCount)
    DirectQuery.cosineSimilarityUpdate(name)
  }

  def calcWordCount(s: String): Map[String, Int] = {
    s.replaceAll( """[^\w가-힣\s]+""", " ").split( """\s+""").filter {
      case "" | "wiki" | "http" | "com" | "www" | "the" | "to" | "of" | "for" | "is" | "a" | "in" | "and" | "be" | "or" => false
      case regexNumber() => false
      case string if 10 < string.length => false
      case _ => true
    }.foldLeft(Map[String, Int]()) {
      (map, word) => map + (word -> (map.getOrElse(word, 0) + 1))
    }
  }

  def updateLink(page:Page) = {
    val pageContent: PageContent = new PageContent(page.content)
    pageContent.interpreter match {
      case Some("Wiki") | Some("wiki") | None =>
        val seqLink = Interpreters.extractLink(page.name, pageContent.content)
        DirectQuery.linkDelete(page.name)
        DirectQuery.linkInsert(seqLink)
      case value =>
    }
  }

  //  def normalizeTokenizeStemFilter(text: String): Seq[String] = {
  //    val nts = TwitterKoreanProcessor.stem(TwitterKoreanProcessor.tokenize(TwitterKoreanProcessor.normalize(text)))
  //    val ntfs = nts.filter(a => Seq(Noun, Adjective, Verb).contains(a.pos)).map(_.text)
  //    ntfs
  //  }

}


