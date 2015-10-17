package logics.wikis.macros

import logics.wikis.interpreters.InterpreterWiki
import models.WikiContext

object MacroNavigation {
  def apply()(implicit wikiContext: WikiContext) = {
    InterpreterWiki.replaceLink(
      "RecentChanges,TitleIndex,PageList,PageMap".split(",")
        .filterNot(_ == wikiContext.name)
        .map(name => s"[$name]")
        .mkString(", ")
    )
  }
}
