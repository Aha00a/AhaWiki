package logics.wikis.macros

import com.aha00a.commons.utils.DateTimeUtil
import logics.wikis.interpreters.ahaMark.AhaMarkLink
import models.ContextWikiPage
import models.tables.CalculatedLink

object MacroDayHeader extends TraitMacro {
  override def isBlock: Boolean = true
  @scala.annotation.tailrec
  override def toHtmlString(argument: String)(implicit wikiContext: ContextWikiPage): String = {
    argument match {
      case "" | null => toHtmlString(wikiContext.nameTop)
      case DateTimeUtil.regexIsoLocalDate(y, m, d) if wikiContext.nameTop == wikiContext.nameBottom =>
        val ymd = s"$y-$m-$d"
        val ymLink = AhaMarkLink(s"$y-$m", s"$y-$m", noFollow = true).toHtmlString()
        val weekday = MacroWeekdayName.toHtmlString(ymd)
        s"""
           |${MacroLinkDate.toHtmlString(ymd)}
           |<h1>${ymLink}-$d $weekday</h1>
           |""".stripMargin
      case DateTimeUtil.regexIsoLocalDate(y, m, d) if wikiContext.nameTop != wikiContext.nameBottom =>
        val ymd = s"$y-$m-$d"
        val weekday = MacroWeekdayName.toHtmlString(ymd)
        s"""
           |<h2>${wikiContext.nameTop} $weekday</h2>
           |""".stripMargin
      case _ => MacroError.toHtmlString(s"Argument Error - [[$name($argument)]]")
    }
  }

  @scala.annotation.tailrec
  override def toSeqLink(body: String)(implicit wikiContext: ContextWikiPage): Seq[CalculatedLink] = body match {
    case "" | null => toSeqLink(wikiContext.name)
    case DateTimeUtil.regexIsoLocalDate(_, _, _) => MacroLinkDate.toSeqLink(body)
    case _ => Seq()
  }
}
