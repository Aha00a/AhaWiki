package logics.wikis.macros

import logics.wikis.PageLogic
import models.WikiContext

object MacroPageCount extends TraitMacro {
  override def apply(argument:String)(implicit wikiContext: WikiContext): String = PageLogic.getListPageWithoutContentWithSize()(wikiContext.request, wikiContext.cacheApi, wikiContext.database).size.toString
}
