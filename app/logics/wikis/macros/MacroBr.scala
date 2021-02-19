package logics.wikis.macros

import models.ContextWikiPage

object MacroBr extends TraitMacro {
  override def toHtmlString(argument: String)(implicit wikiContext: ContextWikiPage): String = "<br/>"
}
