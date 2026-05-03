package logics.wikis.macros

import models.ContextWikiPage

object MacroAdjacentPages extends TraitMacro {
  override def isBlock: Boolean = true
  override def toHtmlString(argument:String)(implicit wikiContext: ContextWikiPage): String = {
    views.html.Wiki.adjacentPagesD3CanvasHost(enableWikiLink = true).toString()
  }
}
