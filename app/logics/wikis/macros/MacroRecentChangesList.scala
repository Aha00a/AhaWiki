package logics.wikis.macros

import models.ContextWikiPage

object MacroRecentChangesList extends TraitMacro {

  override def toHtmlString(argument: String)(implicit wikiContext: ContextWikiPage): String = {
    MacroRecentChanges.toHtmlString("20")
  }
}
