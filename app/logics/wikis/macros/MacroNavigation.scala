package logics.wikis.macros

import logics.DefaultPageLogic
import logics.wikis.interpreters.InterpreterWiki
import models.ContextWikiPage

object MacroNavigation extends TraitMacro{
  override def toHtmlString(argument:String)(implicit wikiContext: ContextWikiPage): String = {
    InterpreterWiki.replaceLink(
      "RecentChanges,TitleIndex,PageList,PageMap,WikiStatistics,schema:Schema".split(",")
        .filter(v => wikiContext.setPageNameByPermission.contains(v) || DefaultPageLogic.isDefined(v))
        .map(name => s"[$name]")
        .mkString(", ")
    )
  }
}
