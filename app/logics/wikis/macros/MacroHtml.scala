package logics.wikis.macros

import models.WikiContext

object MacroHtml extends TraitMacro {
  override def toHtmlString(argument: String)(implicit wikiContext: WikiContext): String = argument
}
