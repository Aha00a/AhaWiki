package logics.wikis.macros

import models.ContextWikiPage

object MacroHtml extends TraitMacro {
  override def toHtmlString(argument: String)(implicit wikiContext: ContextWikiPage): String = argument
}
