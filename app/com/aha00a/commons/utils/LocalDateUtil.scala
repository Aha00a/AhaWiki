package com.aha00a.commons.utils

import java.time.LocalDate
import java.time.Year
import java.time.YearMonth
import scala.util.Try

object LocalDateUtil {
  /** How much of a date was written: a day, a month, or a year alone. */
  sealed trait Precision
  object Precision {
    case object Day extends Precision
    case object Month extends Precision
    case object Year extends Precision
  }

  /**
   * The date `s` writes, and how much of it it writes: `2024-03-05`, `2024-03` or `2024`. A month
   * or a year alone is read as its first day.
   *
   * Until 2026-11-21 the two short forms were tried with `LocalDate.parse` and a formatter that has
   * no day, which always fails -- a LocalDate cannot be made without one -- and the failure was
   * swallowed. So `2024-03` and `1994` were not dates anywhere this was asked.
   */
  def tryParseWithPrecision(s: String): Option[(LocalDate, Precision)] = None
    .orElse(Try(LocalDate.parse(s, DateTimeFormatterHolder.isoLocalDate)).toOption.map(_ -> Precision.Day))
    .orElse(Try(YearMonth.parse(s, DateTimeFormatterHolder.yearDashMonth)).toOption.map(_.atDay(1) -> Precision.Month))
    .orElse(Try(Year.parse(s, DateTimeFormatterHolder.year)).toOption.map(_.atDay(1) -> Precision.Year))

  def tryParse(s: String): Option[LocalDate] = tryParseWithPrecision(s).map(_._1)
}
