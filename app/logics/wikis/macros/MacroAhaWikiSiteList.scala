package logics.wikis.macros

import com.aha00a.commons.Implicits._
import models.ContextWikiPage
import models.tables.Site

object MacroAhaWikiSiteList extends TraitMacro {
  override def isBlock: Boolean = true

  override def toHtmlString(argument: String)(implicit wikiContext: ContextWikiPage): String =
    wikiContext.database.withConnection { implicit connection =>
      render(Site.selectPublicListed())
    }

  override def extractLink(argument: String)(implicit wikiContext: ContextWikiPage): Seq[String] =
    wikiContext.database.withConnection { implicit connection =>
      Site.selectPublicListed().map(siteUrl)
    }

  private[macros] def render(sites: Seq[Site]): String = {
    val fallbackFavicon = "/public/favicon.png".escapeHtmlAttribute()
    val items = sites.map { site =>
      val url = siteUrl(site)
      val href = url.escapeHtmlAttribute()
      val displayName = site.name.escapeHtml()
      val faviconUrl = faviconUrlFor(site).escapeHtmlAttribute()

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

  private def faviconUrlFor(site: Site): String =
    s"${siteUrl(site)}/favicon.ico"
}
