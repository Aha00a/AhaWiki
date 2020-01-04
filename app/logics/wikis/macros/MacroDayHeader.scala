package logics.wikis.macros

import com.aha00a.commons.utils.DateTimeUtil
import logics.wikis.interpreters.InterpreterWiki.LinkMarkup
import logics.wikis.interpreters.Interpreters
import models.WikiContext

object MacroDayHeader extends TraitMacro {
  override def apply(argument: String)(implicit wikiContext: WikiContext): String = {
    (argument, wikiContext.nameTop, wikiContext.nameBottom) match {
      case ("" | null, _, _) => apply(wikiContext.nameTop)
      case (_, DateTimeUtil.regexIsoLocalDate(ty, tm, td), DateTimeUtil.regexIsoLocalDate(by, bm, bd)) => 
        Interpreters.interpret(
          s"""
             |[[LinkDate]]
             |= $argument [[WeekdayName]]
             |""".stripMargin
        )
      case (_, DateTimeUtil.regexIsoLocalDate(ty, tm, td), DateTimeUtil.regexYearDashMonth(by, bm)) =>
        Interpreters.interpret(
          s"""
             |== [${wikiContext.nameTop}] [[WeekdayName]]
             |""".stripMargin
        )
      case _ => MacroError(s"Argument Error - [[$name($argument)]]")
    }
  }

  @scala.annotation.tailrec
  override def extractLink(body: String)(implicit wikiContext: WikiContext): Seq[String] = body match {
    case "" | null => extractLink(wikiContext.name)
    case DateTimeUtil.regexIsoLocalDate(y, m, d) => MacroLinkDate.extractLink(body)
    case _ => Seq()
  }

  def l(s:String)(implicit wikiContext: WikiContext): String = LinkMarkup(s, s.replaceAllLiterally("--", "")).toHtmlString()

}
