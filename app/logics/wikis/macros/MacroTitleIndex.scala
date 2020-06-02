package logics.wikis.macros

import logics.wikis.interpreters.InterpreterWiki
import models.WikiContext

object MacroTitleIndex extends TraitMacro {
  override def toHtmlString(argument:String)(implicit wikiContext: WikiContext): String = { wikiContext.database.withConnection { implicit connection =>
    val listPageName: Seq[String] = wikiContext.seqPageNameByPermission
    InterpreterWiki.toHtmlString {
      listPageName.groupBy(_.charAt(0)).toList.sortBy(_._1).map {
        case (k, v) => s"== $k\n" + v.map(s => s" * [wiki:$s]").mkString("\n")
      }.mkString("\n")
    }}
  }
}
