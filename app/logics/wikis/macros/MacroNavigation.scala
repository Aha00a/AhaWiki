package logics.wikis.macros

import logics.wikis.interpreters.InterpreterWiki
import models.ContextWikiPage

object MacroNavigation extends TraitMacro{
  override def toHtmlString(argument:String)(implicit wikiContext: ContextWikiPage): String = {
    InterpreterWiki.replaceLink(
      "RecentChanges,TitleIndex,PageList,PageMap,WikiStatistics".split(",")
        .filter(wikiContext.setPageNameByPermission.contains)
        .map(name => s"[$name]")
        .mkString(", ")
    )
  }
}
