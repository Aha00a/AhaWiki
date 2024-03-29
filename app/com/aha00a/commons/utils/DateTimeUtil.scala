package com.aha00a.commons.utils

import java.time.{Instant, LocalDate}

import com.aha00a.commons.Implicits._

import scala.util.Try
import scala.util.matching.Regex

object DateTimeUtil {
  lazy val regexIsoLocalDate: Regex = """^([1-9]\d{3})-(0[1-9]|1[0-2])-([0-2]\d|3[0-1])$""".r
  lazy val regexYearDashMonth: Regex = """^([1-9]\d{3})-(0[1-9]|1[0-2])$""".r
  lazy val regexYear: Regex = """^([1-9]\d{3})$""".r
  lazy val regexDashDashMonthDashDay: Regex = """^--(0[1-9]|1[0-2])-([0-2]\d|3[0-1])$""".r
  lazy val regexDashDashMonth: Regex = """^--(0[1-9]|1[0-2])$""".r
  lazy val regexDashDashDashDashDay: Regex = """^----([0-2]\d|3[0-1])$""".r

  def nowEpochMicro: Long = Instant.now().toEpochMilli * 1000

  def expand_ym_to_ym_y(ym:String): Seq[String] = {
    Try(LocalDate.parse(ym, DateTimeFormatterHolder.yearDashMonth)).toOption.map(localDate => Seq(
      localDate.toYearString,
      localDate.toYearDashMonthString,
    )).getOrElse(Seq(ym))
  }
  def expand_ymd_to_ymd_ym_y_md(ymd:String): Seq[String] = {
    Try(LocalDate.parse(ymd, DateTimeFormatterHolder.isoLocalDate)).toOption.map(localDate => Seq(
      localDate.toIsoLocalDateString,
      localDate.toYearDashMonthString,
      localDate.toDashDashMonthDashDayString,
    )).getOrElse(Seq(ymd))
  }

  def getLastDay(month: Int): Int = month match {
    case 1 | 3 | 5 | 7 | 8 | 10 | 12 => 31
    case 4 | 6 | 9 | 11 => 30
    case 2 => 29
    case _ => 0
  }
}
