package logics.wikis.macros

import logics.wikis.interpreters.InterpreterWiki
import models.WikiContext

object MacroTitleIndex extends TraitMacro {
  override def toHtmlString(argument:String)(implicit wikiContext: WikiContext): String = { wikiContext.database.withConnection { implicit connection =>
    val listPageName: Seq[String] = wikiContext.seqPageNameByPermission
    InterpreterWiki.toHtmlString {
      import com.aha00a.commons.utils.LetterUtil
      listPageName.groupBy(LetterUtil.firstLetterForIndex).toList.sortBy(_._1).map {
        case (k, v) => s"== $k\n" + v.map(s => s""" * ["$s"]""").mkString("\n")
      }.mkString("\n")
    }}
  }
}
