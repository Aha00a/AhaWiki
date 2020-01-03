package logics.wikis.macros

import java.time.format.TextStyle
import java.time.{DayOfWeek, YearMonth}

import com.aha00a.commons.Implicits._
import com.aha00a.play.Implicits._
import logics.wikis.interpreters.InterpreterWiki.LinkMarkup
import models.WikiContext
import play.api.cache.CacheApi
import play.api.db.Database

import scala.util.matching.Regex

object MacroCalendar extends TraitMacro {
  val regex: Regex = """^(\d{4})-(\d{2})$""".r

  override def apply(argument: String)(implicit wikiContext: WikiContext): String = argument match {
    case "" | null => apply(wikiContext.name)
    case "-" => apply(wikiContext.name + ",-")
    case regex(y, m) =>
      implicit val cacheApi: CacheApi = wikiContext.cacheApi
      implicit val database: Database = wikiContext.database

      val yearMonth = YearMonth.of(y.toInt, m.toInt)
      val firstPadding: Seq[String] = Seq.fill(yearMonth.atDay(1).getDayOfWeek.getValue - 1)("")
      val lastPadding: Seq[String] = Seq.fill(7 - yearMonth.atEndOfMonth().getDayOfWeek.getValue)("")

      val dates: Seq[String] = (1 to yearMonth.lengthOfMonth()).map(d => LinkMarkup(f"$argument-$d%02d", f"$d%02d").toHtmlString())
      val r = <table class="macroCalendar simpleTable">
        <thead>
          <tr>
            <th colspan="7">{scala.xml.XML.loadString(LinkMarkup(s"$y-$m").toHtmlString())}</th>
          </tr>
          <tr>
            {
              DayOfWeek.values().map(_.getDisplayName(TextStyle.NARROW, wikiContext.request.locale)).map(v =>
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
    case _ => MacroError(s"Argument Error - [[$name($argument)]]")
  }

  @scala.annotation.tailrec
  override def extractLink(body: String)(implicit wikiContext: WikiContext): Seq[String] = body match {
    case "" | null => extractLink(wikiContext.name)
    case "-" => extractLink(wikiContext.name + ",-")
    case regex(y, m) => (1 to YearMonth.of(y.toInt, m.toInt).lengthOfMonth()).map(d => f"$y-${m.toInt}%02d-$d%02d")
    case _ => Seq()
  }
}
