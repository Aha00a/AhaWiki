package logics.wikis.macros

import com.aha00a.commons.Implicits._
import logics.PermissionLogic
import logics.wikis.ExtractConvertInjectMacro
import logics.wikis.HtmlPlaceholders
import logics.wikis.PageLogic
import logics.wikis.interpreters.InterpreterWiki
import models.ContextWikiPage
import models.tables.CalculatedCosineSimilarity
import models.tables.Site
import play.api.Logging

import scala.collection.immutable

object MacroSimilarPages extends TraitMacro with Logging {
  /**
   * The list is AhaMark -- its bullets and numbers are the renderer's -- but no page's name is
   * written into that markup: each line's HTML is made from the values and put in afterwards
   * (HtmlPlaceholders).
   *
   * Until 2026-11-21 a line was written as `[[PercentLinkTitle(0.7, Abbr:Name, "")]]` and the
   * whole list rendered. A similarity row can join pages of two sites, so `Name` could be a page
   * of a sister wiki, named by its author: a name holding `)]]` ended the macro there and the
   * rest of it was read as markup, `[[Html(...)]]` included -- HTML of one wiki's author, run for
   * the readers of another.
   */
  override def toHtmlString(argument:String)(implicit wikiContext: ContextWikiPage): String = {
    val placeholders = new HtmlPlaceholders
    val markup = getMarkupSimilarPages(argument.getOrElse(wikiContext.name), line => placeholders.hold(lineHtml(line)))
    placeholders.restore(InterpreterWiki.toHtmlString(markup))
  }

  /** One line of the list: the page it names (`Abbr:Name` for a sister wiki's), how alike the two are, and the terms they share. */
  private[macros] case class Line(link: String, similarity: Double, terms: String)

  // What `[[PercentLinkTitle(similarity, link, "")]] [[Trivial(terms)]]` came out as, made from the
  // values: the two macros' own HTML, each in the element every macro's HTML is wrapped in.
  private def lineHtml(line: Line)(implicit wikiContext: ContextWikiPage): String =
    ExtractConvertInjectMacro.wrap(MacroPercentLinkTitle, MacroPercentLinkTitle.toHtmlString(line.similarity, line.link, "")) +
      " " +
      ExtractConvertInjectMacro.wrap(MacroTrivial, MacroTrivial.toHtmlString(line.terms))

  private def getMarkupSimilarPages(name: String, draw: Line => String)(implicit wikiContext: ContextWikiPage): String = {
    wikiContext.withConnection { implicit connection =>
      implicit val site: Site = wikiContext.site

      val siteBySeq: Map[Long, Site] = logics.AhaWikiCacheMemoryDomainSite.getSites()(wikiContext.databaseHeldFirst).map(site => site.seq -> site).toMap
      val sameSiteSimilarities: immutable.Seq[CalculatedCosineSimilarity] = CalculatedCosineSimilarity
        .selectSameSite(name)
        .view
        .filter(_.and(wikiContext.pageCanSee))
        .take(20)
        .toSeq

      val crossSiteSimilarities: immutable.Seq[CalculatedCosineSimilarity] = CalculatedCosineSimilarity
        .selectCrossSite(name)
        .view
        .filter(c => siteBySeq.get(c.site2).exists(targetSite => PermissionLogic.anonymousCanRead(targetSite, c.name2)))
        .take(20)
        .toSeq

      val cosineSimilarities = sameSiteSimilarities ++ crossSiteSimilarities
      if (cosineSimilarities.isEmpty) {
        ""
      } else {
        import models.HighScoredTerm

        def toTermString(seq: Seq[HighScoredTerm]): String =
          seq.take(10).map(h => s"${h.term}(${h.frequency1}:${h.frequency2})").mkString(", ")

        val sameSiteHighScoredTerms: Map[(Long, String), String] = {
          val similarPageNames: Seq[String] = sameSiteSimilarities.map(_.name2)
          PageLogic.selectHighScoredTerm(name, similarPageNames)
            .groupBy(h => (wikiContext.site.seq, h.name))
            .view
            .mapValues(toTermString)
            .toMap
        }

        val crossSiteHighScoredTerms: Map[(Long, String), String] = crossSiteSimilarities
          .groupBy(_.site2)
          .flatMap { case (siteSeq, similarities) =>
            siteBySeq.get(siteSeq).toSeq.flatMap { targetSite =>
              PageLogic.selectHighScoredTerm(wikiContext.site, name, targetSite, similarities.map(_.name2))
                .groupBy(h => (targetSite.seq, h.name))
                .view
                .mapValues(toTermString)
                .toMap
            }
          }

        val highScoredTerms = sameSiteHighScoredTerms ++ crossSiteHighScoredTerms
        formatSimilarPagesMarkup(
          sameSiteSimilarities = sameSiteSimilarities,
          crossSiteSimilarities = crossSiteSimilarities,
          siteBySeq = siteBySeq,
          currentSiteSeq = wikiContext.site.seq,
          highScoredTerms = highScoredTerms,
        )(draw)
      }
    }
  }

  /**
   * The list's markup. `draw` gives what stands on a line for the page it names; the macro passes
   * one that answers with a placeholder, so this markup holds no name.
   */
  private[macros] def formatSimilarPagesMarkup(
    sameSiteSimilarities: Seq[CalculatedCosineSimilarity],
    crossSiteSimilarities: Seq[CalculatedCosineSimilarity],
    siteBySeq: Map[Long, Site],
    currentSiteSeq: Long,
    highScoredTerms: Map[(Long, String), String],
  )(draw: Line => String): String = {
    val hasSameSite = sameSiteSimilarities.nonEmpty
    val hasCrossSite = crossSiteSimilarities.nonEmpty

    if (hasSameSite && hasCrossSite) {
      Seq(
        formatSimilarPagesGroup("Same Wiki", sameSiteSimilarities, siteBySeq, currentSiteSeq, highScoredTerms, draw),
        formatSimilarPagesGroup("Sister Wikis", crossSiteSimilarities, siteBySeq, currentSiteSeq, highScoredTerms, draw),
      ).flatten.mkString("\n")
    } else {
      formatSimilarPagesItems(
        similarities = sameSiteSimilarities ++ crossSiteSimilarities,
        siteBySeq = siteBySeq,
        currentSiteSeq = currentSiteSeq,
        highScoredTerms = highScoredTerms,
        indent = " ",
        draw = draw,
      )
    }
  }

  private def formatSimilarPagesGroup(
    heading: String,
    similarities: Seq[CalculatedCosineSimilarity],
    siteBySeq: Map[Long, Site],
    currentSiteSeq: Long,
    highScoredTerms: Map[(Long, String), String],
    draw: Line => String,
  ): Option[String] = {
    if (similarities.isEmpty) {
      None
    } else {
      val links = formatSimilarPagesItems(similarities, siteBySeq, currentSiteSeq, highScoredTerms, indent = "  ", draw = draw)

      Some(s" * $heading\n$links")
    }
  }

  private def formatSimilarPagesItems(
    similarities: Seq[CalculatedCosineSimilarity],
    siteBySeq: Map[Long, Site],
    currentSiteSeq: Long,
    highScoredTerms: Map[(Long, String), String],
    indent: String,
    draw: Line => String,
  ): String = {
    similarities.map { c =>
      val link = siteBySeq
        .get(c.site2)
        .filter(_.seq != currentSiteSeq)
        .map(targetSite => s"${targetSite.abbr}:${c.name2}")
        .getOrElse(c.name2)
      s"${indent}1. ${draw(Line(link, c.similarity, highScoredTerms.getOrElse((c.site2, c.name2), "")))}"
    }.mkString("\n")
  }

}
