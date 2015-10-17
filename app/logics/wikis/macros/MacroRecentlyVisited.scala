package logics.wikis.macros

import logics.wikis.RecentlyVisited
import models.WikiContext

object MacroRecentlyVisited {
  def apply()(implicit wikiContext: WikiContext): String = {
    RecentlyVisited.value(wikiContext.request).map(p => s"""<a href="$p">$p</a>""").mkString(" ")
  }
}
