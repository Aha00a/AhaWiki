package logics.wikis.macros

import java.time.LocalDate
import java.time.Month

import com.aha00a.commons.Implicits._
import com.aha00a.commons.utils.DateTimeUtil
import com.aha00a.commons.utils.RangeUtil
import logics.wikis.interpreters.Interpreters
import logics.wikis.interpreters.ahaMark.AhaMarkLink
import models.ContextWikiPage

object MacroNavigationYearMonth extends TraitMacro {
  @scala.annotation.tailrec
  override def toHtmlString(argument: String)(implicit wikiContext: ContextWikiPage): String = {
    argument match {
      case "" | null => toHtmlString(wikiContext.nameTop)
      case DateTimeUtil.regexYearDashMonth(y, m) =>
        val localDate = LocalDate.of(y.toIntOrZero, Month.of(m.toIntOrZero), 1)
        s"""<div class="rightInfoBox">${RangeUtil.around(0, 12).map(i => AhaMarkLink(localDate.plusMonths(i).toYearDashMonthString, "", noFollow = true).toHtmlString()).mkString("<br/>")}</div>"""
      case _ => MacroError.toHtmlString(s"Argument Error - [[$name($argument)]]")
    }
  }

  @scala.annotation.tailrec
  override def extractLink(body: String)(implicit wikiContext: ContextWikiPage): Seq[String] = body match {
    case "" | null => extractLink(wikiContext.name)
    case DateTimeUtil.regexYearDashMonth(_, _) => MacroLinkDate.extractLink(body)
    case _ => Seq()
  }
}
