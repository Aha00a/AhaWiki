package logics.wikis.macros

import java.time.LocalDate
import java.time.format.TextStyle

import com.aha00a.play.Implicits._
import logics.wikis.macros.MacroDayHeader.name
import models.WikiContext

import scala.util.matching.Regex

object MacroWeekdayName extends TraitMacro {
  val regex: Regex = """^(\d{4}-\d{2}-\d{2})$""".r

  override def apply(argument: String)(implicit wikiContext: WikiContext): String = argument match {
    case "" | null => apply(wikiContext.nameTop)
    case regex(ymd) => LocalDate.parse(ymd).getDayOfWeek.getDisplayName(TextStyle.SHORT, wikiContext.request.locale)
    case _ => MacroError(s"Argument Error - [[$name($argument)]]")
  }
}
