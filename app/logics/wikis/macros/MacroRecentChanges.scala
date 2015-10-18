package logics.wikis.macros

import implicits.Implicits._
import logics.wikis.interpreters.InterpreterWiki
import models.{DirectQuery, WikiContext}

object MacroRecentChanges {
  def apply()(implicit wikiContext: WikiContext): String = {
    new InterpreterWiki().interpret(
      DirectQuery.pageSelectRecentChanges().groupBy(_.localYearMonth).toList.sortBy(_._1)(Ordering[String].reverse).map {
        case (yearMonth, groupedByYearMonth) =>
          s"== $yearMonth\n" +
            groupedByYearMonth.groupBy(_.localDateTime.toLocalDate).toList.sortWith((a, b) => a._1.isAfter(b._1)).map {
              case (yearMonthDate, v) =>
                s"=== $yearMonthDate\n" +
                  v.map(t => {
                    s" * '''[wiki:${t.name}]''' at ${t.localDateTime.toIsoDateTimeString}"
                  }).mkString("\n")
            }.mkString("\n")
      }.mkString("\n")
    )
  }
}
