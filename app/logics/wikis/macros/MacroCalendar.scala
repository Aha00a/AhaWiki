package logics.wikis.macros

import com.aha00a.commons.Implicits._
import logics.wikis.interpreters.ahaMark.AhaMarkLink
import models.ContextWikiPage

import java.time.DayOfWeek
import java.time.YearMonth
import java.time.format.TextStyle

object MacroCalendar extends TraitMacro {
  override def isBlock: Boolean = true
  import com.aha00a.commons.utils.DateTimeUtil._

  override def toHtmlString(argument: String)(implicit wikiContext: ContextWikiPage): String = argument match {
    case "" | null => toHtmlString(wikiContext.name)
    case "-" => toHtmlString(wikiContext.name + ",-")
    case regexYear(y) => (1 to 12).map(m => toHtmlString(f"$y-$m%02d")).mkString("\n")
    case regexYearDashMonth(y, m) =>
      val yearMonth = YearMonth.of(y.toInt, m.toInt)
      val firstPadding: Seq[String] = Seq.fill(yearMonth.atDay(1).getDayOfWeek.getValue - 1)("")
      val lastPadding: Seq[String] = Seq.fill(7 - yearMonth.atEndOfMonth().getDayOfWeek.getValue)("")

      val set: Set[String] = wikiContext.setPageNameByPermission
      val dates: Seq[String] = (1 to yearMonth.lengthOfMonth()).map(d => AhaMarkLink(f"$argument-$d%02d", f"$d%02d", noFollow = true).toHtmlString(set))
      val r = <table class="MacroCalendar simpleTable">
        <thead>
          <tr>
            <th colspan="7">{scala.xml.XML.loadString(AhaMarkLink(s"$y-$m").toHtmlString())}</th>
          </tr>
          <tr>
            {
              DayOfWeek.values().map(_.getDisplayName(TextStyle.NARROW, wikiContext.requestWrapper.locale)).map(v =>
                <th>{v}</th>
              )
            }
          </tr>
        </thead>
        <tbody>
          {(firstPadding ++ dates ++ lastPadding).grouped(7).map(v =>
            <tr>
              {
                v.map(d =>
                  <td>{if(d.isNullOrEmpty) "" else scala.xml.XML.loadString(d)}</td>
                )
              }
            </tr>
          )}
        </tbody>
      </table>
      r.toString
    case _ => MacroError.toHtmlString(s"Argument Error - [[$name($argument)]]")
  }

  @scala.annotation.tailrec
  override def extractLink(body: String)(implicit wikiContext: ContextWikiPage): Seq[String] = body match {
    case "" | null => extractLink(wikiContext.name)
    case "-" => extractLink(wikiContext.name + ",-")
    case regexYear(y) => (1 to 12).map(m => f"$y-$m%02d")
    case regexYearDashMonth(y, m) => (1 to YearMonth.of(y.toInt, m.toInt).lengthOfMonth()).map(d => f"$y-${m.toInt}%02d-$d%02d")
    case _ => Seq()
  }
}
