package logics.wikis.macros

import com.aha00a.commons.Implicits._
import models.WikiContext

import scala.util.matching.Regex

object MacroPercentLinkTitle extends TraitMacro {
  val regex: Regex = """([^,\s]+),\s*([^,]+)(?:,\s*"([^"]*))"""".r
  override def apply(argument: String)(implicit wikiContext: WikiContext): String = argument match {
    case regex(percent, link, title) => views.html.Wiki.percentLinkTitle(percent.toDoubleOrZero, link, title).toString()
  }
}
