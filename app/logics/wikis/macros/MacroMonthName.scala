package logics.wikis.macros

import java.time.Month
import java.time.format.TextStyle

import com.aha00a.commons.utils.DateTimeUtil
import com.aha00a.play.Implicits._
import models.WikiContext

object MacroMonthName extends TraitMacro {
  @scala.annotation.tailrec
  override def toHtmlString(argument: String)(implicit wikiContext: WikiContext): String = argument match {
    case "" | null => toHtmlString(wikiContext.name)
    case DateTimeUtil.regexDashDashMonth(mm) =>
      s"""${Month.of(mm.toInt).getDisplayName(TextStyle.FULL, wikiContext.provider.locale)}"""
  }
}
