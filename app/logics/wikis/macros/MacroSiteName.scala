package logics.wikis.macros

import models.ContextWikiPage

object MacroSiteName extends TraitMacro {
  override def toHtmlString(argument:String)(implicit contextWikiPage: ContextWikiPage): String = contextWikiPage.site.name
}
