package logics.wikis.macros

import com.aha00a.commons.Implicits._
import com.aha00a.commons.utils.IpAddressUtil
import com.aha00a.commons.utils.UriUtil
import logics.wikis.UserPageLogic
import models.ContextWikiPage

object MacroPageList extends TraitMacro {
  override def isBlock: Boolean = true

  override def toHtmlString(argument: String)(implicit wikiContext: ContextWikiPage): String = {
    val sb = new StringBuilder(4096)

    sb.append("<table class=\"simpleTable tablesorter\"><thead><tr>")
      .append("<th>Name</th><th>Date</th><th>Size</th><th>Revision</th><th>Author</th><th>Remote Address</th><th>Comment</th>")
      .append("</tr></thead><tbody>")

    wikiContext.seqPageByPermission.foreach { page =>
      val pageHref = s"/w/${UriUtil.encodeURIComponent(page.name)}"
      val diffHref = s"$pageHref?action=diff&after=${page.revision}"

      sb.append("<tr><td><strong><a href=\"")
        .append(pageHref.escapeHtmlAttribute())
        .append("\">")
        .append(page.name.escapeHtml())
        .append("</a></strong></td><td>")
        .append(page.localDateTime.toIsoLocalDateTimeString.escapeHtml())
        .append("</td><td class=\"text-right\">")
        .append(page.size)
        .append("</td><td><a href=\"")
        .append(diffHref.escapeHtmlAttribute())
        .append("\">")
        .append(page.revision)
        .append("</a></td><td>")
        .append(page.nickname.map(UserPageLogic.wikiMarkup).getOrElse(""))
        .append("</td><td>")
        .append(IpAddressUtil.mask(page.remoteAddress).escapeHtml())
        .append("</td><td>")
        .append(page.comment.escapeHtml())
        .append("</td></tr>")
    }

    sb.append("</tbody></table>").result()
  }
}
