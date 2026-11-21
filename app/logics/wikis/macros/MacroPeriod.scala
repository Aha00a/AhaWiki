package logics.wikis.macros

import com.aha00a.commons.Implicits.RichPeriod
import com.aha00a.commons.Implicits.RichString
import com.aha00a.commons.utils.LocalDateUtil
import com.aha00a.commons.utils.LocalDateUtil.Precision
import models.ContextWikiPage

import java.time.LocalDate
import java.time.Period
import java.time.YearMonth
import java.time.temporal.ChronoUnit

object MacroPeriod extends TraitMacro {
  override def toHtmlString(argument: String)(implicit wikiContext: ContextWikiPage): String = {
    doToHtmlString(argument)(wikiContext.localDateNow)
  }

  def doToHtmlString(argument: String)(implicit localDateNow: LocalDate = LocalDate.now()): String = {
    argument
      .toOption
      .map(_.trim.split("""\s*,\s*""").toSeq.filter(_.isNotNullOrEmpty))
      .getOrElse(Seq[String]())
      .flatMap(v => LocalDateUtil.tryParseWithPrecision(v))
    match {
      case Seq(date) => between((localDateNow, Precision.Day), date)
      case Seq(date1, date2) => between(date1, date2)
      case _ => s"[[$name(${argument.orEmpty})]]"
    }
  }

  def between(localDate1: LocalDate, localDate2: LocalDate): String = {
    Period.between(localDate1, localDate2).toIso8601
  }

  /**
   * The period between two dates, at the coarser of the precisions they were written with: whole
   * years when either is a year alone, years and months when either is a month. A year or a month
   * says nothing about the day, so counting from the first of it would claim days nobody wrote.
   */
  private def between(date1: (LocalDate, Precision), date2: (LocalDate, Precision)): String = {
    val (d1, p1) = date1
    val (d2, p2) = date2
    if (p1 == Precision.Year || p2 == Precision.Year)
      Period.ofYears(math.abs(d2.getYear - d1.getYear)).toIso8601
    else if (p1 == Precision.Month || p2 == Precision.Month)
      Period.ofMonths(math.abs(ChronoUnit.MONTHS.between(YearMonth.from(d1), YearMonth.from(d2)).toInt)).normalized().toIso8601
    else
      between(d1, d2)
  }
}
