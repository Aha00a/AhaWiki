package logics.wikis.macros

import com.aha00a.commons.Implicits._
import models.ContextWikiPage

import scala.util.matching.Regex

object MacroTrivial extends TraitMacro {
  override def toHtmlString(argument: String)(implicit wikiContext: ContextWikiPage): String = <span class="trivial">{argument}</span>.toString
}
