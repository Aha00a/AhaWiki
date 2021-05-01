package logics.wikis.interpreters.ahaMark

import com.aha00a.commons.Implicits._
import com.aha00a.commons.utils.RegexUtil
import models.ContextWikiPage

case class AhaMarkLink(uri: String, alias: String = "")(implicit wikiContext: ContextWikiPage) extends AhaMark {

  import models.tables.Link

  import scala.xml.Elem
  import scala.xml.XML

  lazy val uriNormalized: String = if (uri.startsWith("wiki:")) uri.substring(5) else uri
  lazy val aliasWithDefault: String = if (alias == null || alias.isEmpty) uriNormalized else alias

  def toHtmlString(set: Set[String] = Set[String]()): String = {
    if (wikiContext.name == uri) {
      s"""<b>$aliasWithDefault</b>"""
    } else {
      import com.aha00a.commons.utils.DateTimeUtil
      import logics.DefaultPageLogic
      import logics.wikis.PageNameLogic
      val external: Boolean = PageNameLogic.isExternal(uri)
      val isStartsWithHash = uriNormalized.startsWith("#")
      val isStartsWithQuestionMark = uriNormalized.startsWith("?")
      val href: String = if (external || isStartsWithHash || isStartsWithQuestionMark) uriNormalized else s"/w/$uriNormalized"
      val attrTarget: String = if (external) """ target="_blank" rel="noopener"""" else ""
      val display: String = aliasWithDefault
      val attrCss = if (uriNormalized.startsWith("schema:")) {
        """ class="schema""""
      } else if (
        set.isEmpty ||
        external ||
        isStartsWithHash ||
        isStartsWithQuestionMark ||
        // uriNormalized.matches(DateTimeUtil.regexIsoLocalDate.pattern.pattern()) ||
        uriNormalized.matches(DateTimeUtil.regexYearDashMonth.pattern.pattern()) ||
        uriNormalized.matches(DateTimeUtil.regexDashDashDashDashDay.pattern.pattern()) ||
        uriNormalized.matches(DateTimeUtil.regexYear.pattern.pattern()) ||
        uriNormalized.matches(DateTimeUtil.regexDashDashMonthDashDay.pattern.pattern()) ||
        uriNormalized.matches(DateTimeUtil.regexDashDashMonth.pattern.pattern()) ||
        set.contains(uriNormalized.replaceAll("""[#?].+$""", "")) ||
        wikiContext.database.withConnection { implicit connection =>
          // TODO: remove connection
          DefaultPageLogic.getOption(uriNormalized).isDefined
        }
      ) {
        ""
      } else {
        """ class="missing""""
      }
      "<a href=\"" + RegexUtil.escapeDollar(href.escapeHtml()) + "\"" + attrTarget + attrCss + ">" + RegexUtil.escapeDollar(display.escapeHtml()) + "</a>"
    }
  }

  def toLink(src: String) = Link(src, uriNormalized, alias)

  override def toHtml: Elem = XML.loadString(toHtmlString())
}
