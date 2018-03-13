package logics.wikis.macros

import java.time.{LocalDate, Period}

import com.aha00a.commons.utils.DateTimeFormatterHolder
import models.WikiContext

import scala.util.Try

object MacroPeriod extends TraitMacro {
  private val regex = """(.+)(?:,(\w+))?""".r

  override def apply(argument: String)(implicit wikiContext: WikiContext): String = {
    val array: Array[LocalDate] = argument.split(",").flatMap(t => Try(LocalDate.parse(t.trim, DateTimeFormatterHolder.isoLocalDate)).toOption)
    array match {
      case Array(d1) => toString(Period.between(d1, LocalDate.now()))
      case Array(d1, d2) => toString(Period.between(d1, d2))
      case _ => MacroError(s"Argument Error - [[$name($argument)]]")
    }
  }

  private def toString(duration: Period) = if (!duration.isNegative) {
    "+" + duration
  } else {
    "-" + duration.negated()
  }
}
