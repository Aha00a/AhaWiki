package logics.wikis.macros

import java.time.LocalDate
import java.time.Month

import com.aha00a.commons.Implicits._
import com.aha00a.commons.utils.DateTimeUtil
import com.aha00a.commons.utils.RangeUtil
import logics.wikis.interpreters.ahaMark.AhaMarkLink
import models.ContextWikiPage
import models.tables.CalculatedLink

object MacroNavigationYear extends TraitMacro {
  override def isBlock: Boolean = true
  @scala.annotation.tailrec
  override def toHtmlString(argument: String)(implicit wikiContext: ContextWikiPage): String = {
    argument match {
      case "" | null => toHtmlString(wikiContext.name)
      case DateTimeUtil.regexYear(y) =>
        s"""<div class="rightInfoBox">${RangeUtil.around(y.toInt, 10).map(y => AhaMarkLink(y.toString, "", noFollow = true).toHtmlString()).mkString("<br/>")}</div>"""
      case _ => argumentError(argument)
    }
  }

  @scala.annotation.tailrec
  override def toSeqLink(body: String)(implicit wikiContext: ContextWikiPage): Seq[CalculatedLink] = body match {
    case "" | null => toSeqLink(wikiContext.name)
    case DateTimeUtil.regexYear(_) => MacroLinkDate.toSeqLink(body)
    case _ => Seq()
  }
}
