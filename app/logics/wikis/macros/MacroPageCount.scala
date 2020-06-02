package logics.wikis.macros

import logics.wikis.PageLogic
import models.WikiContext

object MacroPageCount extends TraitMacro {
  override def toHtmlString(argument:String)(implicit wikiContext: WikiContext): String = wikiContext.listPageByPermission.size.toString
}
