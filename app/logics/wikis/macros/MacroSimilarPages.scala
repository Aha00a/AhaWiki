package logics.wikis.macros

import actors.ActorAhaWiki.Calculate
import com.aha00a.commons.Implicits._
import logics.wikis.interpreters.InterpreterWiki
import models.WikiContext

import scala.collection.immutable

object MacroSimilarPages extends TraitMacro {
  override def toHtmlString(argument:String)(implicit wikiContext: WikiContext): String = {
    InterpreterWiki.toHtmlString(getMarkupSimilarPages(argument.getOrElse(wikiContext.nameTop)))
  }

  def getMarkupSimilarPages(name: String)(implicit wikiContext: WikiContext): String = {
    wikiContext.database.withConnection { implicit connection =>
      import models.tables.CosineSimilarity
      val cosineSimilarities: immutable.Seq[CosineSimilarity] = CosineSimilarity.select(name).filter(v => v.and(wikiContext.pageCanSee))
      if (cosineSimilarities.isEmpty) {
        wikiContext.actorAhaWiki ! Calculate(name)
        ""
      } else {
        import models.tables.TermFrequency
        val similarPageNames = cosineSimilarities.map(_.name2)
        val highScoredTerms = TermFrequency.selectHighScoredTerm(name, similarPageNames).groupBy(_.name).view.mapValues(_.map(_.term).mkString(", ")).toMap
        cosineSimilarities.map(c => s""" * [[PercentLinkTitle(${c.similarity}, ${c.name2}, "${highScoredTerms.getOrElse(c.name2, "")}")]]""").mkString("\n")
      }
    }
  }

}
