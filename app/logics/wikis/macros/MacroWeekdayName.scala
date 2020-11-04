package logics.wikis.macros

import java.time.LocalDate
import java.time.format.TextStyle

import models.WikiContext

import scala.util.matching.Regex

object MacroWeekdayName extends TraitMacro {
  val regex: Regex = """^(\d{4}-\d{2}-\d{2})$""".r

  @scala.annotation.tailrec
  override def toHtmlString(argument: String)(implicit wikiContext: WikiContext): String = argument match {
    case "" | null => toHtmlString(wikiContext.nameTop)
    case regex(ymd) => LocalDate.parse(ymd).getDayOfWeek.getDisplayName(TextStyle.SHORT, wikiContext.provider.locale)
    case _ => MacroError.toHtmlString(s"Argument Error - [[$name($argument)]]")
  }
}
