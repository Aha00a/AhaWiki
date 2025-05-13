package logics.wikis.macros

import com.aha00a.commons.PeriodDuration
import com.aha00a.commons.utils.LocalDateTimeUtil
import models.ContextWikiPage

import java.time.LocalDateTime

object MacroPeriod extends TraitMacro {
  override def toHtmlString(argument: String)(implicit wikiContext: ContextWikiPage): String = {
    argument.split(",") match {
      case Array(s1) =>
        LocalDateTimeUtil.tryParse(s1)
          .map(d => PeriodDuration.between(d, LocalDateTime.now()).toISO8601)
          .getOrElse(MacroError.toHtmlString(s"Argument Error - [[$name($argument)]]"))
      case Array(d1, d2) =>
        LocalDateTimeUtil.tryParse(d1)
          .flatMap(d1 => LocalDateTimeUtil.tryParse(d2).map(d2 => PeriodDuration.between(d1, d2)))
          .map(_.toISO8601)
          .getOrElse(MacroError.toHtmlString(s"Argument Error - [[$name($argument)]]"))
      case _ =>
        MacroError.toHtmlString(s"Argument Error - [[$name($argument)]]")
    }
  }
}
