package logics.wikis.macros

import logics.wikis.interpreters.InterpreterWiki
import models.WikiContext

object MacroNavigation extends TraitMacro{
  override def toHtmlString(argument:String)(implicit wikiContext: WikiContext): String = {
    InterpreterWiki.replaceLink(
      "RecentChanges,TitleIndex,PageList,PageMap,WikiStatistics".split(",")
        .filter(wikiContext.setPageNameByPermission.contains)
        .map(name => s"[$name]")
        .mkString(", ")
    )
  }
}
