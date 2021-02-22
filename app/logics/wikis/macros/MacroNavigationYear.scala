package logics.wikis.macros

import java.time.LocalDate
import java.time.Month

import com.aha00a.commons.Implicits._
import com.aha00a.commons.utils.DateTimeUtil
import com.aha00a.commons.utils.RangeUtil
import logics.wikis.interpreters.ahaMark.AhaMarkLink
import models.ContextWikiPage

object MacroNavigationYear extends TraitMacro {
  @scala.annotation.tailrec
  override def toHtmlString(argument: String)(implicit wikiContext: ContextWikiPage): String = {
    argument match {
      case "" | null => toHtmlString(wikiContext.nameTop)
      case DateTimeUtil.regexYear(y) =>
        s"""<div class="rightInfoBox">${RangeUtil.around(y.toInt, 10).map(y => AhaMarkLink(y.toString).toHtmlString()).mkString("<br/>")}</div>"""
      case _ => MacroError.toHtmlString(s"Argument Error - [[$name($argument)]]")
    }
  }

  @scala.annotation.tailrec
  override def extractLink(body: String)(implicit wikiContext: ContextWikiPage): Seq[String] = body match {
    case "" | null => extractLink(wikiContext.name)
    case DateTimeUtil.regexYear(_) => MacroLinkDate.extractLink(body)
    case _ => Seq()
  }
}
