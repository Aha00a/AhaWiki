package logics.wikis.macros

import com.aha00a.commons.Implicits._
import models.ContextWikiPage
import models.tables.Site

import scala.util.matching.Regex

object MacroPercentLinkTitle extends TraitMacro {
  val regex: Regex = """([^,\s]+),\s*([^,]+)(?:,\s*"([^"]*))"""".r

  override def toHtmlString(argument: String)(implicit wikiContext: ContextWikiPage): String =
    toHtmlString(argument, link => resolveLink(link))

  private[macros] def toHtmlString(argument: String, pageNames: Set[String], sites: Seq[Site]): String =
    toHtmlString(argument, link => resolveLink(link, pageNames, sites))

  private def toHtmlString(argument: String, resolve: String => ResolvedLink): String = argument match {
    case regex(percent, link, title) =>
      val resolvedLink = resolve(link)
      val percentString = "%2.2f".format(percent.toDoubleOrZero * 100) + "%"
      s"""<a href="${resolvedLink.href.escapeHtmlAttribute()}"${targetAttributes(resolvedLink.external)}><span class="percentTotal">
         |        <span class="percentBar" style="width: ${percentString.escapeHtmlAttribute()}"></span>
         |        <span class="percentLabel">${percentString.escapeHtml()}</span>
         |    </span>${resolvedLink.displayLink.escapeHtml()}</a>
         |    <span>${title.escapeHtml()}</span>""".stripMargin
  }

  private val twinLinkRegex: Regex = """([^:]+):(.+)""".r

  private[macros] case class ResolvedLink(href: String, displayLink: String, external: Boolean)

  private def resolveLink(link: String)(implicit wikiContext: ContextWikiPage): ResolvedLink =
    resolveLink(link, wikiContext.setPageName, () => logics.AhaWikiCacheMemoryDomainSite.getSites()(wikiContext.database))

  private[macros] def resolveLink(link: String, pageNames: Set[String], sites: Seq[Site]): ResolvedLink = {
    resolveLink(link, pageNames, () => sites)
  }

  private def resolveLink(link: String, pageNames: Set[String], sites: () => Seq[Site]): ResolvedLink = {
    if (pageNames.contains(link)) {
      ResolvedLink(controllers.routes.Wiki.view(link).url, link, external = false)
    } else {
      link match {
        case twinLinkRegex(abbr, pageName) =>
          val targetSite = sites().find(site => site.abbr == abbr && site.mainDomain.nonEmpty)

          targetSite
            .map(site => ResolvedLink(s"https://${site.mainDomain}/w/${encodePageName(pageName)}", link, external = true))
            .getOrElse(ResolvedLink(controllers.routes.Wiki.view(link).url, link, external = false))
        case _ =>
          ResolvedLink(controllers.routes.Wiki.view(link).url, link, external = false)
      }
    }
  }

  private[macros] def targetAttributes(external: Boolean): String =
    if (external) """ target="_blank" rel="noopener"""" else ""

  private def encodePageName(pageName: String): String =
    java.net.URLEncoder.encode(pageName, "UTF-8").replace("+", "%20")
}
