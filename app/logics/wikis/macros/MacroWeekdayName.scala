package logics.wikis.macros

import java.time.LocalDate
import java.time.format.TextStyle

import models.ContextWikiPage

import scala.util.matching.Regex

object MacroWeekdayName extends TraitMacro {
  val regex: Regex = """^(\d{4}-\d{2}-\d{2})$""".r

  @scala.annotation.tailrec
  override def toHtmlString(argument: String)(implicit wikiContext: ContextWikiPage): String = argument match {
    case "" | null => toHtmlString(wikiContext.nameTop)
    case regex(ymd) => LocalDate.parse(ymd).getDayOfWeek.getDisplayName(TextStyle.SHORT, wikiContext.requestWrapper.locale)
    case _ => MacroError.toHtmlString(s"Argument Error - [[$name($argument)]]")
  }
}
