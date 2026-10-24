package logics.wikis.macros

import com.aha00a.commons.Implicits._
import com.aha00a.commons.utils.DateTimeUtil
import logics.wikis.interpreters.InterpreterSchema
import logics.wikis.interpreters.ahaMark.AhaMarkLink
import models.ContextWikiPage
import models.tables.CalculatedSchemaOrg
import models.tables.Site

/**
 * The pages whose date properties fall on one month and day, whatever the year.
 *
 * This reads what the `#!Schema` blocks already put in the database — `birthDate 1815-12-10`
 * on a page makes that page an entry for December 10th — so nothing has to be written anywhere
 * for a page to appear. A date written without a year, `--10-23`, is an entry too: it is how a
 * birthday is written when the year is not the point, and it is listed after the dated ones.
 *
 * It answers "which pages have a date on this day", not "what happened". A page that was written
 * on the day, as a day page is, is a different question and not this macro's.
 */
object MacroOnThisDay extends TraitMacro {
  override def isBlock: Boolean = true

  /** A page, the property that put it on this day, and the year — absent when the date had none. */
  case class Entry(year: Option[Int], prop: String, page: String)

  override def toHtmlString(argument: String)(implicit wikiContext: ContextWikiPage): String =
    monthDay(argument) match {
      case None => argumentError(argument)
      case Some((month, day)) =>
        val entries = wikiContext.withConnection { implicit connection =>
          implicit val site: Site = wikiContext.site
          toEntries(CalculatedSchemaOrg.selectWhereDateOn(month, day))
        }.filter(e => wikiContext.pageCanSee(e.page))

        if (entries.isEmpty) ""
        else entries.map(toListItem).mkString("<ul class=\"OnThisDay\">", "", "</ul>")
    }

  /**
   * The month and day this call is about, as two-digit strings.
   *
   * With no argument it is the page's own day when the page is named for one — `2026-10-24` or
   * `--10-24` — and today otherwise, so the same line placed on a day page, on a `--MM-DD` page
   * and on the front page each says what is natural there. A full date is accepted and its year
   * ignored, which lets a day page pass its own name along.
   */
  def monthDay(argument: String)(implicit wikiContext: ContextWikiPage): Option[(String, String)] =
    argument match {
      case "" | null => parse(wikiContext.name).orElse(Some(f"${wikiContext.localDateNow.getMonthValue}%02d" -> f"${wikiContext.localDateNow.getDayOfMonth}%02d"))
      case _ => parse(argument)
    }

  private def parse(s: String): Option[(String, String)] = s match {
    case DateTimeUtil.regexDashDashMonthDashDay(m, d) => Some(m -> d)
    case DateTimeUtil.regexIsoLocalDate(_, m, d) => Some(m -> d)
    case _ => None
  }

  /**
   * Dated entries oldest first, then the undated ones, each group by page name.
   *
   * The same page appears once per property, not once per class it declares — the database holds a
   * row per class, and a page that is both a Person and a Thing was not born twice.
   */
  def toEntries(rows: Seq[CalculatedSchemaOrg]): Seq[Entry] =
    rows
      .filter(r => InterpreterSchema.isDateKey(r.prop))
      .map(r => Entry(yearOf(r.value), r.prop, r.page))
      .distinct
      .sortBy(e => (e.year.isEmpty, e.year.getOrElse(0), e.page, e.prop))

  private def yearOf(value: String): Option[Int] = value match {
    case DateTimeUtil.regexIsoLocalDate(y, _, _) => Some(y.toInt)
    case _ => None
  }

  private def toListItem(e: Entry)(implicit wikiContext: ContextWikiPage): String = {
    val year = e.year.map(_.toString).getOrElse("")
    s"""<li><span class="year">$year</span> <span class="prop">${e.prop.escapeHtml()}</span> ${AhaMarkLink(e.page, noFollow = true).toHtmlString()}</li>"""
  }
}
