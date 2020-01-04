package logics.wikis.macros

import actors.ActorAhaWiki.Calculate
import logics.wikis.interpreters.InterpreterWiki
import com.aha00a.commons.Implicits._
import models.{AhaWikiQuery, CosineSimilarity, WikiContext}

import scala.collection.immutable

object MacroSimilarPages extends TraitMacro {
  override def apply(argument:String)(implicit wikiContext: WikiContext): String = {
    InterpreterWiki(getMarkupSimilarPages(argument.toOption.getOrElse(wikiContext.nameTop)))
  }

  def getMarkupSimilarPages(name: String)(implicit wikiContext: WikiContext): String = {
    wikiContext.database.withConnection { implicit connection =>
      val ahaWikiQuery = AhaWikiQuery()
      val cosineSimilarities: immutable.Seq[CosineSimilarity] = ahaWikiQuery.CosineSimilarity.select(name)
      if (cosineSimilarities.isEmpty) {
        wikiContext.actorAhaWiki ! Calculate(name)
        ""
      } else {
        val similarPageNames = cosineSimilarities.map(_.name2)
        val highScoredTerms = ahaWikiQuery.selectHighScoredTerm(name, similarPageNames).groupBy(_.name).mapValues(_.map(_.term).mkString(", "))
        cosineSimilarities.map(c => s""" * [[PercentLinkTitle(${c.similarity}, ${c.name2}, "${highScoredTerms.getOrElse(c.name2, "")}")]]""").mkString("\n")
      }
    }
  }

}
