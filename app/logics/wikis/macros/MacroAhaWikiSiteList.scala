package logics.wikis.macros

import com.aha00a.commons.Implicits._
import logics.AhaWikiConfig
import models.ContextWikiPage
import models.tables.CalculatedLink
import models.tables.Site

object MacroAhaWikiSiteList extends TraitMacro {
  override def isBlock: Boolean = true

  override def toHtmlString(argument: String)(implicit wikiContext: ContextWikiPage): String =
    wikiContext.withConnection { implicit connection =>
      render(Site.selectPublicListed())
    }

  override def toSeqLink(argument: String)(implicit wikiContext: ContextWikiPage): Seq[CalculatedLink] =
    toCalculatedLinks(wikiContext.withConnection { implicit connection =>
      Site.selectPublicListed().map(siteUrl)
    })

  // Each site's icon is its own /favicon.ico, which that site answers by sending the request on to
  // the favicon it has configured (Home.favicon). Until 2026-10-03 this macro read every listed
  // site's Config itself and resolved it with its own copy of AhaWikiConfig's rules, and fell back
  // to /favicon.ico for the rest -- a route that did not exist, so every unconfigured site in the
  // list cost a failed request before the onerror fallback below took over. The fallback stays for
  // a site that does not answer.
  private[macros] def render(sites: Seq[Site]): String = {
    val fallbackFavicon = AhaWikiConfig.DefaultFaviconPath.escapeHtmlAttribute()
    val items = sites.map { site =>
      val url = siteUrl(site)
      val href = url.escapeHtmlAttribute()
      val displayName = site.name.escapeHtml()
      val faviconUrl = s"$url/favicon.ico".escapeHtmlAttribute()

      s"""<li><a href="$href" target="_blank" rel="noopener"><img src="$faviconUrl" alt="" loading="lazy" onerror="this.onerror=null;this.src='$fallbackFavicon';"/>$displayName</a></li>"""
    }.mkString

    s"""<ul class="MacroAhaWikiSiteList">$items</ul>"""
  }

  private[macros] def publicSites(sites: Seq[Site]): Seq[Site] =
    sites
      .filter(site => site.publicListedOrder.exists(_ > 0) && site.mainDomain.trim.nonEmpty)
      .sortBy(site => (-site.publicListedOrder.getOrElse(BigDecimal(0)), site.seq))

  private[macros] def siteUrl(site: Site): String =
    s"https://${site.mainDomain}"
}
