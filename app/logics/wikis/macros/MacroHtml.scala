package logics.wikis.macros

import models.WikiContext

object MacroHtml extends TraitMacro {
  override def apply(argument: String)(implicit wikiContext: WikiContext): String = argument
}
