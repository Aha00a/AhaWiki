package logics.wikis.macros

import com.aha00a.commons.Implicits._
import models.WikiContext

import scala.util.matching.Regex

object MacroTrivial extends TraitMacro {
  override def toHtmlString(argument: String)(implicit wikiContext: WikiContext): String = <span class="trivial">{argument}</span>.toString
}
