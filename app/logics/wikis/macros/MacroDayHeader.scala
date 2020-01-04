package logics.wikis.macros

import java.time.LocalDate

import com.aha00a.commons.Implicits._
import com.aha00a.commons.utils.{DateTimeUtil, RangeUtil}
import logics.wikis.interpreters.InterpreterWiki.LinkMarkup
import logics.wikis.interpreters.Interpreters
import models.WikiContext

object MacroDayHeader extends TraitMacro {
  override def apply(argument: String)(implicit wikiContext: WikiContext): String = argument match {
    case "" | null => apply(wikiContext.name)
    case DateTimeUtil.regexYearDashMonth(y, m) => ""
    case DateTimeUtil.regexIsoLocalDate(y, m, d) =>
      Interpreters.interpret(
        s"""
           |[[LinkDate]]
           |= $argument [[WeekdayName]]
           |""".stripMargin
      )
    case _ => MacroError(s"Argument Error - [[$name($argument)]]")
  }

  @scala.annotation.tailrec
  override def extractLink(body: String)(implicit wikiContext: WikiContext): Seq[String] = body match {
    case "" | null => extractLink(wikiContext.name)
    case DateTimeUtil.regexIsoLocalDate(y, m, d) => MacroLinkDate.extractLink(body)
    case _ => Seq()
  }

  def l(s:String)(implicit wikiContext: WikiContext): String = LinkMarkup(s, s.replaceAllLiterally("--", "")).toHtmlString()

}
