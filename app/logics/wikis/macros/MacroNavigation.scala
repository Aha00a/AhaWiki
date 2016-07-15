package logics.wikis.macros

import logics.wikis.interpreters.InterpreterWiki
import models.WikiContext

object MacroNavigation extends TraitMacro{
  override def apply(argument:String)(implicit wikiContext: WikiContext) = {
    InterpreterWiki.replaceLink(
      "RecentChanges,TitleIndex,PageList,PageMap".split(",")
        .filter(_ != wikiContext.name)
        .map(name => s"[$name]")
        .mkString(", ")
    )
  }
}
