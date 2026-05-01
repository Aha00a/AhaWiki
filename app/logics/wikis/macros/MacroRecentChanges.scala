package logics.wikis.macros

import models.ContextWikiPage
import views.html.macros

object MacroRecentChanges extends TraitMacro {

  override def toHtmlString(argument: String)(implicit wikiContext: ContextWikiPage): String = {
    macros.RecentChanges(wikiContext).toString
  }
}
