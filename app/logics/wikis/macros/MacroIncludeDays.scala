package logics.wikis.macros

import logics.wikis.interpreters.Interpreters
import models.ContextWikiPage
import models.PageContent
import models.tables.CalculatedLink
import models.tables.Site
import play.api.db.Database

import java.time.YearMonth
import scala.util.matching.Regex

object MacroIncludeDays extends TraitMacro {
  override def isBlock: Boolean = true
  val regex: Regex = """^(\d{4})-(\d{2})$""".r

  //noinspection ScalaUnusedSymbol
  @scala.annotation.tailrec
  override def toHtmlString(argument: String)(implicit wikiContext: ContextWikiPage): String = argument match {
    case "" | null => toHtmlString(wikiContext.name)
    case "-" => toHtmlString(wikiContext.name + ",-")
    case regex(y, m) =>
      implicit val database: Database = wikiContext.database
      implicit val site: Site = wikiContext.site

      val set = wikiContext.setPageNameByPermission
      val seq = getSeqDays_yyyy_dash_MM_dash_dd(y.toInt, m.toInt).filter(set.contains)
      if (seq.isEmpty) {
        ""
      } else {
        // One query for the month, then one render per day. The render is what has to be per
        // day: each one goes through a context pushed onto the include stack, so the day page's
        // edit links, attachment keys and structured data name the day page rather than the
        // month page around it.
        wikiContext.database.withConnection { implicit connection =>
          models.tables.Page
            .selectLastRevision(seq)
            .map(p => Interpreters.toHtmlString(withDayHeader(p.content))(wikiContext.push(p.name)))
            .mkString("\n")
        }
      }
    case _ => argumentError(argument)
  }

  /**
   * A day page's body under a `DayHeader`, whatever heading it happened to carry itself.
   *
   * Day pages are not written the same way. Most call `[[DayHeader]]`; the rest open with the
   * same thing spelled out, `= [2020-01]-04 Sat`. Both mean "this is the heading for when I am
   * read on my own", and on a month page both should come out as one section among thirty. So
   * the first line goes, if it was a heading, and `DayHeader` takes its place — which under a
   * pushed context knows to render a section heading rather than a page heading.
   *
   * Only a heading is dropped. Taking the first line whatever it was, as this did until now,
   * was safe only because no day page has ever opened with prose, and would have eaten the
   * first sentence of the one that did. Directives go through `PageContent`, which knows a page
   * may carry more than one of them.
   */
  private def withDayHeader(raw: String): String = {
    val lines = PageContent(raw).content.split("\n").toSeq
    val ownHeading = lines.headOption.exists(l => l.startsWith("=") || l.contains(s"[[${MacroDayHeader.name}"))
    val body = if (ownHeading) lines.tail else lines
    (s"[[${MacroDayHeader.name}]]" +: body).mkString("\n")
  }

  override def toSeqLink(body: String)(implicit wikiContext: ContextWikiPage): Seq[CalculatedLink] =
    toCalculatedLinks(linkDestinations(body))

  @scala.annotation.tailrec
  // See the note in MacroCalendar: same name and shape, different expansion, deliberately apart.
  private def linkDestinations(body: String)(implicit wikiContext: ContextWikiPage): Seq[String] = body match {
    case "" | null => linkDestinations(wikiContext.name)
    case "-" => linkDestinations(wikiContext.name + ",-")
    case regex(y, m) => (1 to YearMonth.of(y.toInt, m.toInt).lengthOfMonth()).map(d => f"$y-${m.toInt}%02d-$d%02d")
    case _ => Seq()
  }

  private def getSeqDays(y: Int, m: Int): Seq[Int] = 1 to YearMonth.of(y, m).lengthOfMonth()
  private def getSeqDays_yyyy_dash_MM_dash_dd(y: Int, m: Int): Seq[String] = getSeqDays(y, m).map(d => f"$y%04d-$m%02d-$d%02d")
}
