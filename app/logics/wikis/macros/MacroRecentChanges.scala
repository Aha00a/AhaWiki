package logics.wikis.macros

import com.aha00a.supercsv.SupercsvUtil
import logics.wikis.interpreters.InterpreterWiki
import models.WikiContext

object MacroRecentChanges extends TraitMacro {
  override def toHtmlString(argument:String)(implicit wikiContext: WikiContext): String = {
    def desc[T : Ordering]: Ordering[T] = implicitly[Ordering[T]].reverse
    InterpreterWiki.toHtmlString(
      wikiContext.listPageByPermission.groupBy(_.year).toList.sortBy(_._1)(desc).map {
        case (year, groupedByYear) =>
          s"== $year\n" +
            groupedByYear.groupBy(_.yearDashMonth).toList.sortBy(_._1)(desc).map {
              case (yearMonth, groupedByYearMonth) =>
                val list = groupedByYearMonth.sortBy(_.dateTime)(desc).map(t => Seq(
                    s"""'''["${t.name}"]'''""",
                    s"""["${t.name}?action=diff&after=${t.revision}" ${t.revision}]""",
                    s"${t.toIsoLocalDateTimeString}",
                    s"[${t.author}](${t.remoteAddress})",
                    s"${t.comment}"
                ))
                s"""=== $yearMonth
                   |[[[#!Table tsv 1 tablesorter
                   |Name	Revision	at	by	comment
                   |${SupercsvUtil.toTsvString(list)}
                   |]]]
                   |""".stripMargin
            }.mkString("\n")
      }.mkString("\n")
    )
  }
}
