package logics.wikis.macros

import com.aha00a.commons.Implicits._
import models.ContextWikiPage

import scala.util.matching.Regex

object MacroPercentLinkTitle extends TraitMacro {
  val regex: Regex = """([^,\s]+),\s*([^,]+)(?:,\s*"([^"]*))"""".r
  override def toHtmlString(argument: String)(implicit wikiContext: ContextWikiPage): String = argument match {
    case regex(percent, link, title) => views.html.Wiki.percentLinkTitle(percent.toDoubleOrZero, link, title).toString()
  }
}
