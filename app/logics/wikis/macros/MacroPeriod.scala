package logics.wikis.macros

import com.aha00a.commons.Implicits.RichPeriod
import com.aha00a.commons.utils.LocalDateUtil
import models.ContextWikiPage

import java.time.Period

object MacroPeriod extends TraitMacro {
  override def toHtmlString(argument: String)(implicit wikiContext: ContextWikiPage): String = {
    argument.split(",").map(s => LocalDateUtil.tryParse(s)) match {
      case Array(Some(localDate)) => Period.between(wikiContext.localDateNow, localDate).toIso8601
      case Array(Some(localDate1), Some(localDate2)) => Period.between(localDate1, localDate2).toIso8601
      case _ => MacroError.toHtmlString(s"Argument Error - [[$name($argument)]]")
    }
  }
}
