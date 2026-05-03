package logics.wikis.macros

import models.ContextWikiPage

object MacroWikiStatistics extends TraitMacro {
  override def isBlock: Boolean = true
  override def toHtmlString(argument: String)(implicit wikiContext: ContextWikiPage): String = {
    views.html.macros.WikiStatistics().toString()
  }
}
