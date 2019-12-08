package logics.wikis.macros

import logics.AhaWikiCache
import models.WikiContext

object MacroPageCount extends TraitMacro {
  override def apply(argument:String)(implicit wikiContext: WikiContext): String = AhaWikiCache.PageList.get()(wikiContext.cacheApi, wikiContext.database).size.toString
}
