package logics.wikis.macros

import java.time.{LocalDate, Period}

import com.aha00a.commons.utils.DateTimeFormatterHolder
import models.WikiContext

import scala.util.Try

object MacroPeriod extends TraitMacro {
  override def toHtmlString(argument: String)(implicit wikiContext: WikiContext): String = {
    argument.split(",").flatMap(t => Try(LocalDate.parse(t.trim, DateTimeFormatterHolder.isoLocalDate)).toOption) match {
      case Array(d1) => toString(Period.between(d1, LocalDate.now()))
      case Array(d1, d2) => toString(Period.between(d1, d2))
      case _ => MacroError.toHtmlString(s"Argument Error - [[$name($argument)]]")
    }
  }

  private def toString(duration: Period) = if (!duration.isNegative) {
    "+" + duration
  } else {
    "-" + duration.negated()
  }
}
