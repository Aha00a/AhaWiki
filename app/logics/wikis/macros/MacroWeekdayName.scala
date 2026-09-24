package logics.wikis.macros

import java.time.LocalDate
import java.time.format.TextStyle

import models.ContextWikiPage

import scala.util.Try
import scala.util.matching.Regex

object MacroWeekdayName extends TraitMacro {
  val regex: Regex = """^(\d{4}-\d{2}-\d{2})$""".r

  @scala.annotation.tailrec
  override def toHtmlString(argument: String)(implicit wikiContext: ContextWikiPage): String = argument match {
    case "" | null => toHtmlString(wikiContext.name)
    // The shape matching is not the same as the date existing: 2026-13-45 matches and then
    // LocalDate.parse threw, which took the whole page to 500 rather than showing the error box
    // an unreadable argument gets everywhere else (MacroMonthName had the same hole in September).
    case regex(ymd) =>
      Try(LocalDate.parse(ymd))
        .map(_.getDayOfWeek.getDisplayName(TextStyle.SHORT, wikiContext.requestWrapper.locale))
        .getOrElse(argumentError(argument))
    case _ => argumentError(argument)
  }
}
