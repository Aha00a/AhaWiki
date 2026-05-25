package logics.wikis.macros

import com.aha00a.commons.Implicits._
import com.aha00a.commons.utils.UriUtil
import models.ContextWikiPage

object MacroPageList extends TraitMacro {
  override def isBlock: Boolean = true

  override def toHtmlString(argument: String)(implicit wikiContext: ContextWikiPage): String = {
    val sb = new StringBuilder(4096)

    sb.append("<table class=\"wikiTableSimple tablesorter\"><thead><tr>")
      .append("<th>Name</th><th>DateTime</th><th>Size</th><th>Revision</th>")
      .append("</tr></thead><tbody>")

    wikiContext.seqPageByPermission.foreach { page =>
      val pageHref = s"/w/${UriUtil.encodeURIComponent(page.name)}"
      val diffHref = s"$pageHref?action=diff&after=${page.revision}"

      sb.append("<tr><td><strong><a href=\"")
        .append(pageHref.escapeHtmlAttribute())
        .append("\">")
        .append(page.name.escapeHtml())
        .append("""</a></strong></td><td class="dateTime">""")
        .append(page.localDateTime.toIsoLocalDateTimeString.escapeHtml())
        .append("</td><td class=\"size\">")
        .append(page.size)
        .append("</td><td class=\"revision\"><a href=\"")
        .append(diffHref.escapeHtmlAttribute())
        .append("\">")
        .append(page.revision)
        .append("</a></td></tr>")
    }

    sb.append("</tbody></table>").result()
  }
}
