package logics.wikis.macros

import com.aha00a.commons.utils.DateTimeUtil
import logics.wikis.interpreters.ahaMark.AhaMarkLink
import logics.wikis.interpreters.Interpreters
import models.ContextWikiPage

object MacroDayHeader extends TraitMacro {
  @scala.annotation.tailrec
  override def toHtmlString(argument: String)(implicit wikiContext: ContextWikiPage): String = {
    argument match {
      case "" | null => toHtmlString(wikiContext.nameTop)
      case DateTimeUtil.regexIsoLocalDate(y, m, d) if wikiContext.nameTop == wikiContext.nameBottom =>
        Interpreters.toHtmlString(
          s"""
             |[[LinkDate($y-$m-$d)]]
             |= ["$y-$m"]-$d [[WeekdayName($y-$m-$d)]]
             |""".stripMargin
        )
      case DateTimeUtil.regexIsoLocalDate(y, m, d) if wikiContext.nameTop != wikiContext.nameBottom =>
        Interpreters.toHtmlString(
          s"""
             |== [${wikiContext.nameTop}] [[WeekdayName($y-$m-$d)]]
             |""".stripMargin
        )
      case _ => MacroError.toHtmlString(s"Argument Error - [[$name($argument)]]")
    }
  }

  @scala.annotation.tailrec
  override def extractLink(body: String)(implicit wikiContext: ContextWikiPage): Seq[String] = body match {
    case "" | null => extractLink(wikiContext.name)
    case DateTimeUtil.regexIsoLocalDate(_, _, _) => MacroLinkDate.extractLink(body)
    case _ => Seq()
  }
}
