package logics.wikis.macros

import actors.ActorAhaWiki.Calculate
import com.aha00a.commons.Implicits._
import logics.wikis.interpreters.InterpreterWiki
import models.WikiContext
import play.api.Logging

import scala.collection.immutable

object MacroSimilarPages extends TraitMacro with Logging {
  override def toHtmlString(argument:String)(implicit wikiContext: WikiContext): String = {
    InterpreterWiki.toHtmlString(getMarkupSimilarPages(argument.getOrElse(wikiContext.nameTop)))
  }

  def getMarkupSimilarPages(name: String)(implicit wikiContext: WikiContext): String = {
    wikiContext.database.withConnection { implicit connection =>
      import models.tables.CosineSimilarity
      import models.tables.Site
      implicit val site: Site = wikiContext.site

      val cosineSimilarities: immutable.Seq[CosineSimilarity] = CosineSimilarity
        .select(name)
        .view
        .filter(_.and(wikiContext.pageCanSee))
        .take(20)
        .toSeq
      if (cosineSimilarities.isEmpty) {
        wikiContext.actorAhaWiki ! Calculate(wikiContext.site, name)
        ""
      } else {
        import models.tables.HighScoredTerm
        import models.tables.Site
        import models.tables.TermFrequency
        implicit val site: Site = wikiContext.site

        val similarPageNames: Seq[String] = cosineSimilarities.map(_.name2)
        val seqHighScoredTerm: Seq[HighScoredTerm] = TermFrequency.selectHighScoredTerm(name, similarPageNames)
        val mapHighScoredTerm: Map[String, Seq[HighScoredTerm]] = seqHighScoredTerm.groupBy(_.name)
        val highScoredTerms: Map[String, String] = mapHighScoredTerm.view.mapValues(_.map(h => s"${h.term}(${h.frequency1}:${h.frequency2})").mkString(", ")).toMap
        cosineSimilarities.map(c => s""" 1. [[PercentLinkTitle(${c.similarity}, ${c.name2}, "")]] [[Trivial(${{highScoredTerms.getOrElse(c.name2, "")}})]]""").mkString("\n")
      }
    }
  }

}
