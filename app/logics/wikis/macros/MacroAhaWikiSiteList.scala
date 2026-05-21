package logics.wikis.macros

import com.aha00a.commons.Implicits._
import logics.ApplicationConf
import models.ContextWikiPage
import models.tables.Config
import models.tables.Site

object MacroAhaWikiSiteList extends TraitMacro {
  private val defaultFavicon = "/public/favicon.png"
  private val faviconConfigKey = "site.favicon.objectKey"

  override def isBlock: Boolean = true

  override def toHtmlString(argument: String)(implicit wikiContext: ContextWikiPage): String =
    wikiContext.database.withConnection { implicit connection =>
      val sites = Site.selectPublicListed()
      render(sites, faviconUrlsFor(sites, wikiContext.applicationConf))
    }

  override def extractLink(argument: String)(implicit wikiContext: ContextWikiPage): Seq[String] =
    wikiContext.database.withConnection { implicit connection =>
      Site.selectPublicListed().map(siteUrl)
    }

  private[macros] def render(sites: Seq[Site]): String =
    render(sites, Map.empty)

  private[macros] def render(sites: Seq[Site], faviconUrlsBySiteSeq: Map[Long, String]): String = {
    val fallbackFavicon = defaultFavicon.escapeHtmlAttribute()
    val items = sites.map { site =>
      val url = siteUrl(site)
      val href = url.escapeHtmlAttribute()
      val displayName = site.name.escapeHtml()
      val faviconUrl = faviconUrlsBySiteSeq.getOrElse(site.seq, defaultFaviconUrlFor(site)).escapeHtmlAttribute()

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

  private def defaultFaviconUrlFor(site: Site): String =
    s"${siteUrl(site)}/favicon.ico"

  private def faviconUrlsFor(sites: Seq[Site], applicationConf: ApplicationConf)(implicit connection: java.sql.Connection): Map[Long, String] =
    sites.flatMap { site =>
      implicit val configSite: Site = site
      Config.select(faviconConfigKey)
        .map(_.v.trim)
        .filter(_.nonEmpty)
        .flatMap(resolveConfiguredFavicon(_, applicationConf))
        .map(site.seq -> _)
    }.toMap

  private def resolveConfiguredFavicon(v: String, applicationConf: ApplicationConf): Option[String] = {
    if (v.startsWith("/") || v.startsWith("http://") || v.startsWith("https://")) {
      Some(v)
    } else {
      S3AttachmentUrlLogic.generatePresignedUrl(applicationConf, v).toOption
    }
  }
}
