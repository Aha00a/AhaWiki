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
                s"=== $yearMonthDate\n[[[#!Table tsv 1\nPage\tat\tby\tcomment\n" +
                  v.sortBy(_.time)(Ordering[Long].reverse).map(t => {
                    s"'''[wiki:${t.name}]'''\t${t.localDateTime.toIsoTimeString}\t[${t.author}]\t${t.comment.getOrElse(" ")}"
                  }).mkString("\n") + "\n]]]"
            }.mkString("\n")
      }.mkString("\n")
    )
  }
}
