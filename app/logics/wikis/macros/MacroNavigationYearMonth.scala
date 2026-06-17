package logics.wikis.macros

import java.time.LocalDate
import java.time.Month

import com.aha00a.commons.Implicits._
import com.aha00a.commons.utils.DateTimeUtil
import com.aha00a.commons.utils.RangeUtil
import logics.wikis.interpreters.Interpreters
import logics.wikis.interpreters.ahaMark.AhaMarkLink
import models.ContextWikiPage
import models.tables.CalculatedLink

object MacroNavigationYearMonth extends TraitMacro {
  override def isBlock: Boolean = true
  @scala.annotation.tailrec
  override def toHtmlString(argument: String)(implicit wikiContext: ContextWikiPage): String = {
    argument match {
      case "" | null => toHtmlString(wikiContext.nameTop)
      case DateTimeUtil.regexYearDashMonth(y, m) =>
        val localDate = LocalDate.of(y.toIntOrZero, Month.of(m.toIntOrZero), 1)
        s"""<div class="rightInfoBox">${RangeUtil.around(0, 3).map(i => AhaMarkLink(localDate.plusMonths(i).toYearDashMonthString, "", noFollow = true).toHtmlString()).mkString("<br/>")}</div>"""
      case _ => MacroError.toHtmlString(s"Argument Error - [[$name($argument)]]")
    }
  }

  @scala.annotation.tailrec
  override def toSeqLink(body: String)(implicit wikiContext: ContextWikiPage): Seq[CalculatedLink] = body match {
    case "" | null => toSeqLink(wikiContext.name)
    case DateTimeUtil.regexYearDashMonth(_, _) => MacroLinkDate.toSeqLink(body)
    case _ => Seq()
  }
}
