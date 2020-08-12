package logics.wikis.interpreters.ahaMark

import com.aha00a.commons.utils.RegexUtil
import models.WikiContext

case class LinkMarkup(uri: String, alias: String = "")(implicit wikiContext: WikiContext) {

  import models.tables.Link

  lazy val uriNormalized: String = if (uri.startsWith("wiki:")) uri.substring(5) else uri
  lazy val aliasWithDefault: String = if (alias == null || alias.isEmpty) uriNormalized else alias

  def toHtmlString(set: Set[String] = Set[String]()): String = {
    if (wikiContext.name == uri) {
      s"""<b>$aliasWithDefault</b>"""
    } else {
      import com.aha00a.commons.utils.DateTimeUtil
      import logics.wikis.PageNameLogic
      val external: Boolean = PageNameLogic.isExternal(uri)
      val href: String = if (uriNormalized.startsWith("schema:")) s"./$uriNormalized" else uriNormalized
      val attrTarget: String = if (external) """ target="_blank"""" else ""
      val display: String = aliasWithDefault
      val attrCss = if (uriNormalized.startsWith("schema:")) {
        """ class="schema""""
      } else if (
        set.isEmpty ||
          external ||
          uriNormalized.startsWith("#") ||
          uriNormalized.startsWith("?") ||
          // uriNormalized.matches(DateTimeUtil.regexIsoLocalDate.pattern.pattern()) ||
          uriNormalized.matches(DateTimeUtil.regexYearDashMonth.pattern.pattern()) ||
          uriNormalized.matches(DateTimeUtil.regexDashDashDashDashDay.pattern.pattern()) ||
          uriNormalized.matches(DateTimeUtil.regexYear.pattern.pattern()) ||
          uriNormalized.matches(DateTimeUtil.regexDashDashMonthDashDay.pattern.pattern()) ||
          uriNormalized.matches(DateTimeUtil.regexDashDashMonth.pattern.pattern()) ||
          set.contains(uriNormalized.replaceAll("""[#?].+$""", ""))
      ) {
        ""
      } else {
        """ class="missing""""
      }
      s"""<a href="${RegexUtil.escapeDollar(href)}"$attrTarget$attrCss>${RegexUtil.escapeDollar(display)}</a>"""
    }
  }

  def toLink(src: String) = Link(src, uriNormalized, alias)
}
