package logics.wikis.macros

import logics.Cache
import models.WikiContext

object MacroPageCount extends TraitMacro {
  override def apply(argument:String)(implicit wikiContext: WikiContext): String = Cache.PageList.get()(wikiContext.cacheApi, wikiContext.db).size.toString
}
