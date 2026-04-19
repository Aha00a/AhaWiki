package logics.wikis.macros

import com.aha00a.commons.utils.IpAddressUtil
import com.aha00a.supercsv.SupercsvUtil
import logics.wikis.UserPageLogic
import logics.wikis.interpreters.InterpreterWiki
import models.ContextWikiPage

object MacroRecentChanges extends TraitMacro {
  private def shouldIncludeMinorEdit(argument: String): Either[String, Boolean] = {
    Option(argument).map(_.trim.toLowerCase).filter(_.nonEmpty) match {
      case None => Right(false)
      case Some("minor") | Some("include-minor") | Some("include_minor") | Some("includeminor") => Right(true)
      case Some(other) => Left(other)
    }
  }

  override def toHtmlString(argument:String)(implicit wikiContext: ContextWikiPage): String = {
    shouldIncludeMinorEdit(argument) match {
      case Left(_) => MacroError.toHtmlString(s"Bad argument - [[$name($argument)]]")
      case Right(includeMinorEdit) =>
        def desc[T : Ordering]: Ordering[T] = implicitly[Ordering[T]].reverse
        val pageList = wikiContext.seqPageByPermission.filter(p => includeMinorEdit || !p.isMinorEdit)
        InterpreterWiki.toHtmlString(
          pageList.groupBy(_.year).toList.sortBy(_._1)(desc).map {
            case (year, groupedByYear) =>
              s"== $year\n" +
                groupedByYear.groupBy(_.yearDashMonth).toList.sortBy(_._1)(desc).map {
                  case (yearMonth, groupedByYearMonth) =>
                    val list = groupedByYearMonth.sortBy(_.dateTime)(desc).map(t => Seq(
                      s"""'''["${t.name}"]'''""",
                      s"""["${t.name}?action=diff&after=${t.revision}" ${t.revision}]""",
                      s"${t.toIsoLocalDateTimeString}",
                      s"${t.nickname.map(UserPageLogic.wikiMarkup).getOrElse("")} ${IpAddressUtil.mask(t.remoteAddress)}",
                      s"${if (t.isMinorEdit) "[minor] " else ""}${t.comment}"
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
}
