package logics.wikis.macros

import logics.AhaWikiCache
import logics.wikis.interpreters.InterpreterWiki
import models.WikiContext

object MacroRecentChanges extends TraitMacro {
  override def apply(argument:String)(implicit wikiContext: WikiContext): String = {
    def desc[T : Ordering] = implicitly[Ordering[T]].reverse
    new InterpreterWiki()(
      AhaWikiCache.PageList.get()(wikiContext.cacheApi, wikiContext.database).groupBy(_.year).toList.sortBy(_._1)(desc).map {
        case (year, groupedByYear) =>
          s"== $year\n" +
            groupedByYear.groupBy(_.yearDashMonth).toList.sortBy(_._1)(desc).map {
              case (yearMonth, groupedByYearMonth) =>
                s"=== $yearMonth\n[[[#!Table tsv 1\nName\tRevision\tat\tby\tcomment\n" +
                  groupedByYearMonth.sortBy(_.time)(desc).map(t => {
                    Seq(
                      s"'''[${t.name}]'''",
                      s"""[[Html(<a href="${t.name}?action=diff&after=${t.revision}">${t.revision}</a>)]]""",
                      s"${t.isoLocalDateTime}",
                      s"[${t.author}](${t.remoteAddress})",
                      s"${t.comment.getOrElse(" ")}"
                    ).mkString("\t")
                  }).mkString("\n") +
                  "\n]]]"
            }.mkString("\n")
      }.mkString("\n")
    )
  }
}
