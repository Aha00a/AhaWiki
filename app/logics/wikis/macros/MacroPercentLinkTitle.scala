package logics.wikis.macros

import com.aha00a.commons.Implicits._
import models.ContextWikiPage

import scala.util.matching.Regex

object MacroPercentLinkTitle extends TraitMacro {
  val regex: Regex = """([^,\s]+),\s*([^,]+)(?:,\s*"([^"]*))"""".r
  override def toHtmlString(argument: String)(implicit wikiContext: ContextWikiPage): String = argument match {
    case regex(percent, link, title) =>
      val (href, displayLink) = resolveLink(link)
      val percentString = "%2.2f".format(percent.toDoubleOrZero * 100) + "%"
      s"""<a href="${href.escapeHtmlAttribute()}"><span class="percentTotal">
         |        <span class="percentBar" style="width: ${percentString.escapeHtmlAttribute()}"></span>
         |        <span class="percentLabel">${percentString.escapeHtml()}</span>
         |    </span>${displayLink.escapeHtml()}</a>
         |    <span>${title.escapeHtml()}</span>""".stripMargin
  }

  private val twinLinkRegex: Regex = """([^:]+):(.+)""".r

  private def resolveLink(link: String)(implicit wikiContext: ContextWikiPage): (String, String) = {
    if (wikiContext.setPageName.contains(link)) {
      (controllers.routes.Wiki.view(link).url, link)
    } else {
      link match {
        case twinLinkRegex(abbr, pageName) =>
          val targetSite = logics.AhaWikiCacheMemoryDomainSite
            .getSites()(wikiContext.database)
            .find(site => site.abbr == abbr && site.mainDomain.nonEmpty)

          targetSite
            .map(site => (s"https://${site.mainDomain}/w/${encodePageName(pageName)}", link))
            .getOrElse((controllers.routes.Wiki.view(link).url, link))
        case _ =>
          (controllers.routes.Wiki.view(link).url, link)
      }
    }
  }

  private def encodePageName(pageName: String): String =
    java.net.URLEncoder.encode(pageName, "UTF-8").replace("+", "%20")
}
