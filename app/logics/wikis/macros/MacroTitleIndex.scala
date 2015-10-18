package logics.wikis.macros

import logics.wikis.interpreters.InterpreterWiki
import models.{DirectQuery, WikiContext}

object MacroTitleIndex {
  def apply()(implicit wikiContext: WikiContext): String = {
    val listPageName = DirectQuery.pageSelectNameGroupByNameOrderByName
    new InterpreterWiki().interpret {
      listPageName.groupBy(_.charAt(0)).toList.sortBy(_._1).map {
        case (k, v) => s"== $k\n" + v.map(s => s" * [wiki:$s]").mkString("\n")
      }.mkString("\n")
    }
  }
}
