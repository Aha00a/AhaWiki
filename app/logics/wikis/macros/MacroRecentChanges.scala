package logics.wikis.macros

import implicits.Implicits._
import logics.Cache
import logics.wikis.interpreters.InterpreterWiki
import models.WikiContext

object MacroRecentChanges {
  def apply()(implicit wikiContext: WikiContext): String = {
    new InterpreterWiki().interpret(
      Cache.PageList.get().groupBy(_.localYearMonth).toList.sortBy(_._1)(Ordering[String].reverse).map {
        case (yearMonth, groupedByYearMonth) =>
          s"== $yearMonth\n" +
            groupedByYearMonth.groupBy(_.localDateTime.toLocalDate).toList.sortWith((a, b) => a._1.isAfter(b._1)).map {
              case (yearMonthDate, v) =>
                s"=== $yearMonthDate\n[[[#!Table tsv 1\nPage\tRevision\tat\tby\tcomment\n" +
                  v.sortBy(_.time)(Ordering[Long].reverse).map(t => {
                    Seq(
                      s"'''[${t.name}]'''",
                      s"""[[Html(<a href="${t.name}?action=diff&after=${t.revision}">${t.revision}</a>)]]""",
                      s"${t.localDateTime.toIsoTimeString}",
                      s"[${t.author}]",
                      s"${t.comment.getOrElse(" ")}"
                    ).mkString("\t")
                  }).mkString("\n") + "\n]]]"
            }.mkString("\n")
      }.mkString("\n")
    )
  }
}
