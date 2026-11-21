package logics.wikis.macros

import logics.wikis.PageNameUrl
import com.aha00a.commons.Implicits._
import logics.AhaWikiCacheMemoryDomainSite
import logics.PermissionLogic
import logics.wikis.HtmlPlaceholders
import logics.wikis.interpreters.InterpreterWiki
import logics.wikis.interpreters.ahaMark.AhaMarkLink
import models.ContextWikiPage
import models.tables.PageMeta
import models.tables.Site

import java.sql.Connection

/**
 * The same page on the sister wikis: the sites that hold a page of this name, which an
 * anonymous reader could open there.
 *
 * [[twinSites]] is the one answer, asked twice per view: `Wiki.getAhaMarkAdditionalInfo` asks
 * whether there are any, to decide if the Twin Pages section is generated at all, and this
 * macro asks which, to draw it. Until 2026-11-01 the view had a copy of the question that
 * did not check `mainDomain`, so a site without an address made the view generate a section
 * the macro then drew empty.
 */
object MacroTwinPages extends TraitMacro {
  private[macros] case class TwinPage(site: Site, pageName: String)

  override def isBlock: Boolean = true

  /**
   * The list is AhaMark, but the link on each line is made from the values and put in afterwards
   * (HtmlPlaceholders), so the name is never read as markup.
   *
   * Until 2026-11-21 a line was written as `[address Abbr:name]` and rendered. For a page that is
   * not on this wiki the name is whatever the address says, so a link someone made was enough to
   * have this list drawn under a name of their choosing -- one holding `]` and `[[Html(...)]]`,
   * with a page of that name on a sister wiki of their own -- and the HTML ran, on this wiki, for
   * whoever followed the link.
   */
  override def toHtmlString(argument: String)(implicit wikiContext: ContextWikiPage): String = {
    val pageName = argument.getOrElse(wikiContext.name)
    val placeholders = new HtmlPlaceholders
    val markup = wikiContext.withConnection { implicit connection =>
      twinSites(pageName).map { targetSite =>
        val link = AhaMarkLink(url(targetSite, pageName), s"${targetSite.abbr}:$pageName").toHtmlString()
        s" 1. ${placeholders.hold(link)}"
      }.mkString("\n")
    }

    markup.toOption.map(listMarkup => placeholders.restore(InterpreterWiki.toHtmlString(listMarkup))).getOrElse("")
  }

  /** The sites, other than this one, where `pageName` exists and an anonymous reader may read it. */
  def twinSites(pageName: String)(implicit wikiContext: ContextWikiPage, connection: Connection): Seq[Site] = {
    val sitesHavingPage = PageMeta.selectSiteSeqsHavingPage(pageName, wikiContext.site.seq)
      .flatMap(siteSeq => AhaWikiCacheMemoryDomainSite.getSite(siteSeq)(wikiContext.databaseHeldFirst))
    collectTwinPages(pageName, sitesHavingPage, wikiContext.site)(
      anonymousCanRead = targetSite => PermissionLogic.anonymousCanRead(targetSite, pageName),
    ).map(_.site)
  }

  /** `sitesHavingPage` are the sites where the page exists; this decides which of them may be shown. */
  private[macros] def collectTwinPages(
    pageName: String,
    sitesHavingPage: Seq[Site],
    currentSite: Site,
  )(
    anonymousCanRead: Site => Boolean,
  ): Seq[TwinPage] = {
    sitesHavingPage.flatMap { targetSite =>
      // A site with no mainDomain has no address to link to -- url() would build `https:///w/...`.
      if (targetSite.seq == currentSite.seq || targetSite.mainDomain.isEmpty || !anonymousCanRead(targetSite)) {
        None
      } else {
        Some(TwinPage(targetSite, pageName))
      }
    }
  }

  private def url(site: Site, pageName: String): String =
    s"https://${site.mainDomain}/w/${PageNameUrl.encode(pageName)}"
}
