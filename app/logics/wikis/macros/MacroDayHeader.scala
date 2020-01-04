package logics.wikis.macros

import com.aha00a.commons.utils.DateTimeUtil
import logics.wikis.interpreters.InterpreterWiki.LinkMarkup
import logics.wikis.interpreters.Interpreters
import models.WikiContext

object MacroDayHeader extends TraitMacro {
  override def apply(argument: String)(implicit wikiContext: WikiContext): String = {
    argument match {
      case "" | null => apply(wikiContext.nameTop)
      case DateTimeUtil.regexIsoLocalDate(y, m, d) if wikiContext.nameTop == wikiContext.nameBottom =>
        Interpreters.interpret(
          s"""
             |[[LinkDate($y-$m-$d)]]
             |= [[Html(${LinkMarkup(s"$y-$m").toHtmlString()})]]-$d [[WeekdayName($y-$m-$d)]]
             |""".stripMargin
        )
      case DateTimeUtil.regexIsoLocalDate(y, m, d) if wikiContext.nameTop != wikiContext.nameBottom =>
        Interpreters.interpret(
          s"""
             |== [${wikiContext.nameTop}] [[WeekdayName($y-$m-$d)]]
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