package logics.wikis.macros

import logics.Cache
import models.WikiContext

object MacroPageList {
  def apply()(implicit wikiContext: WikiContext) = {
    views.html.Wiki.pageList(Cache.PageList.get()).toString()
  }
}
