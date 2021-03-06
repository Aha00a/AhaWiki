package logics.wikis.macros

import logics.wikis.interpreters.InterpreterWiki
import models.ContextWikiPage

object MacroNavigation extends TraitMacro{
  override def toHtmlString(argument:String)(implicit wikiContext: ContextWikiPage): String = {
    import logics.DefaultPageLogic
    InterpreterWiki.replaceLink(
      "RecentChanges,TitleIndex,PageList,PageMap,WikiStatistics,schema:Schema".split(",")
        .filter(v => wikiContext.setPageNameByPermission.contains(v) || wikiContext.database.withConnection { implicit connection =>
          // TODO: remove connection
          DefaultPageLogic.getOption(v).isDefined
        })
        .map(name => s"[$name]")
        .mkString(", ")
    )
  }
}
