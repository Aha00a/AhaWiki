package logics.wikis.macros

import logics.wikis.PageLogic
import models.WikiContext

object MacroPageCount extends TraitMacro {
  override def apply(argument:String)(implicit wikiContext: WikiContext): String = wikiContext.listPageByPermission.size.toString
}
