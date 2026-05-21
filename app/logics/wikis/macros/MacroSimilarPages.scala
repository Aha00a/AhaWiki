package logics.wikis.macros

import actors.ActorAhaWiki.Calculate
import com.aha00a.commons.Implicits._
import logics.AhaWikiCacheMemoryPermission
import logics.PermissionLogic
import logics.wikis.PageLogic
import logics.wikis.interpreters.InterpreterWiki
import models.ContextWikiPage
import models.tables.CalculatedCosineSimilarity
import models.tables.Permission
import models.tables.Site
import play.api.Logging

import scala.collection.immutable

object MacroSimilarPages extends TraitMacro with Logging {
  override def toHtmlString(argument:String)(implicit wikiContext: ContextWikiPage): String = {
    InterpreterWiki.toHtmlString(getMarkupSimilarPages(argument.getOrElse(wikiContext.nameTop)))
  }

  def getMarkupSimilarPages(name: String)(implicit wikiContext: ContextWikiPage): String = {
    wikiContext.database.withConnection { implicit connection =>
      implicit val site: Site = wikiContext.site

      val siteBySeq: Map[Long, Site] = logics.AhaWikiCacheMemoryDomainSite.getSites()(wikiContext.database).map(site => site.seq -> site).toMap
      def anonymousCanRead(targetSite: Site, pageName: String): Boolean = {
        val permissionLogic = new PermissionLogic(AhaWikiCacheMemoryPermission.get()(connection, targetSite))
        permissionLogic.permitted(pageName, "", Permission.Action.Read.id)
      }

      val sameSiteSimilarities: immutable.Seq[CalculatedCosineSimilarity] = CalculatedCosineSimilarity
        .selectSameSite(name)
        .view
        .filter(_.and(wikiContext.pageCanSee))
        .take(20)
        .toSeq

      val crossSiteSimilarities: immutable.Seq[CalculatedCosineSimilarity] = CalculatedCosineSimilarity
        .selectCrossSite(name)
        .view
        .filter(c => siteBySeq.get(c.site2).exists(targetSite => anonymousCanRead(targetSite, c.name2)))
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
        )
      }
    }
  }

  private[macros] def formatSimilarPagesMarkup(
    sameSiteSimilarities: Seq[CalculatedCosineSimilarity],
    crossSiteSimilarities: Seq[CalculatedCosineSimilarity],
    siteBySeq: Map[Long, Site],
    currentSiteSeq: Long,
    highScoredTerms: Map[(Long, String), String],
  ): String = {
    val hasSameSite = sameSiteSimilarities.nonEmpty
    val hasCrossSite = crossSiteSimilarities.nonEmpty

    if (hasSameSite && hasCrossSite) {
      Seq(
        formatSimilarPagesGroup("Same Wiki", sameSiteSimilarities, siteBySeq, currentSiteSeq, highScoredTerms),
        formatSimilarPagesGroup("Sister Wikis", crossSiteSimilarities, siteBySeq, currentSiteSeq, highScoredTerms),
      ).flatten.mkString("\n")
    } else {
      formatSimilarPagesItems(
        similarities = sameSiteSimilarities ++ crossSiteSimilarities,
        siteBySeq = siteBySeq,
        currentSiteSeq = currentSiteSeq,
        highScoredTerms = highScoredTerms,
        indent = " ",
      )
    }
  }

  private def formatSimilarPagesGroup(
    heading: String,
    similarities: Seq[CalculatedCosineSimilarity],
    siteBySeq: Map[Long, Site],
    currentSiteSeq: Long,
    highScoredTerms: Map[(Long, String), String],
  ): Option[String] = {
    if (similarities.isEmpty) {
      None
    } else {
      val links = formatSimilarPagesItems(similarities, siteBySeq, currentSiteSeq, highScoredTerms, indent = "  ")

      Some(s" * $heading\n$links")
    }
  }

  private def formatSimilarPagesItems(
    similarities: Seq[CalculatedCosineSimilarity],
    siteBySeq: Map[Long, Site],
    currentSiteSeq: Long,
    highScoredTerms: Map[(Long, String), String],
    indent: String,
  ): String = {
    similarities.map { c =>
      val link = siteBySeq
        .get(c.site2)
        .filter(_.seq != currentSiteSeq)
        .map(targetSite => s"${targetSite.abbr}:${c.name2}")
        .getOrElse(c.name2)
      s"""${indent}1. [[PercentLinkTitle(${c.similarity}, $link, "")]] [[Trivial(${{highScoredTerms.getOrElse((c.site2, c.name2), "")}})]]"""
    }.mkString("\n")
  }

}
