package logics.wikis.macros

import com.aha00a.commons.Implicits.RichPeriod
import com.aha00a.commons.utils.LocalDateUtil
import models.ContextWikiPage

import java.time.Period

object MacroPeriod extends TraitMacro {
  override def toHtmlString(argument: String)(implicit wikiContext: ContextWikiPage): String = {
    argument.split(",").map(s => LocalDateUtil.tryParse(s)) match {
      case Array(Some(ld)) =>
        val period = Period.between(wikiContext.now, ld)
        val timeQualifier = if(period.isZero) "" else if(period.isNegative) s"${period.abs} ago" else s"$period hence"
        timeQualifier
      case Array(Some(ld1), Some(ld2)) =>
        val period = Period.between(ld1, ld2)
        if(period.isZero) "" else period.abs.toString
      case _ => MacroError.toHtmlString(s"Argument Error - [[$name($argument)]]")
    }
  }
}
