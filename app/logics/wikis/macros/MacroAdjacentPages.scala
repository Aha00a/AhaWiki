package logics.wikis.macros

import models.ContextWikiPage

object MacroAdjacentPages extends TraitMacro {
  override def toHtmlString(argument:String)(implicit wikiContext: ContextWikiPage): String = {
    views.html.Wiki.adjacentPages(enableWikiLink = true).toString()
  }
}
