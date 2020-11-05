package logics.wikis.macros

import com.aha00a.commons.Implicits._
import logics.wikis.interpreters.InterpreterTable
import models.WikiContext

object MacroPageList extends TraitMacro {
  override def toHtmlString(argument: String)(implicit wikiContext: WikiContext): String = {
    import com.aha00a.supercsv.SupercsvUtil
    val th = Seq("Name", "Date", "Size", "Revision", "Author", "Remote Address", "Comment").mkString("\t")
    val table: Seq[Seq[String]] = wikiContext.listPageByPermission.map(t => Seq(
      s"""'''["${t.name}"]'''""",
      s"${t.localDateTime.toIsoLocalDateTimeString}",
      s"${t.size}",
      s"""[[Html(<a href="${t.name}?action=diff&after=${t.revision}">${t.revision}</a>)]]""",
      s"[${t.author}]",
      s"${t.remoteAddress}",
      s"${t.comment}"
    ))

    InterpreterTable.toHtmlString(
      s"""#!Table tsv 1 tablesorter
         |$th
         |${SupercsvUtil.toTsvString(table)}
         |""".stripMargin
    )
  }
}
