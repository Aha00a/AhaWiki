package logics.wikis.macros

import models.WikiContext

object MacroWikiStatistics extends TraitMacro {
  override def toHtmlString(argument: String)(implicit wikiContext: WikiContext): String = {
    views.html.macros.WikiStatistics().toString()
  }
}
