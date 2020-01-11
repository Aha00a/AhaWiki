package logics.wikis.macros

import com.aha00a.commons.Implicits._
import logics.wikis.PageLogic
import logics.wikis.interpreters.InterpreterTable
import models.{PageContent, WikiContext}

object MacroPageList extends TraitMacro {
  override def apply(argument: String)(implicit wikiContext: WikiContext): String = InterpreterTable.interpret(PageContent(
    "#!Table tsv 1 tablesorter\nName\tDate\tSize\tRevision\tAuthor\tRemote Address\tComment\n" +
      wikiContext.listPageByPermission.map { t =>
      Seq(
        s"'''[${t.name}]'''",
        s"${t.localDateTime.toIsoLocalDateTimeString}",
        s"${t.size}",
        s"""[[Html(<a href="${t.name}?action=diff&after=${t.revision}">${t.revision}</a>)]]""",
        s"[${t.author}]",
        s"${t.remoteAddress}",
        s"${t.comment}"
      ).mkString("\t")
    }.mkString("\n")
  ))
}
