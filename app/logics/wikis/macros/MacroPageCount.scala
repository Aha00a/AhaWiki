package logics.wikis.macros

import logics.wikis.PageLogic
import models.ContextWikiPage

object MacroPageCount extends TraitMacro {
  override def toHtmlString(argument:String)(implicit wikiContext: ContextWikiPage): String = wikiContext.listPageByPermission.size.toString
}
