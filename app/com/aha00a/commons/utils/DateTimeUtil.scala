package com.aha00a.commons.utils

import java.time.{Instant, LocalDate}

import com.aha00a.commons.Implicits._

import scala.util.Try
import scala.util.matching.Regex

object DateTimeUtil {
  lazy val regexIsoLocalDate: Regex = """^(\d{4})-(0[1-9]|1[0-2])-([0-2]\d|3[0-1])$""".r
  lazy val regexYearDashMonth: Regex = """^(\d{4})-(0[1-9]|1[0-2])$""".r
  lazy val regexYear: Regex = """^(\d{4})$""".r
  lazy val regexDashDashMonthDashDay: Regex = """^--(0[1-9]|1[0-2])-([0-2]\d|3[0-1])$""".r
  lazy val regexDashDashMonth: Regex = """^--(0[1-9]|1[0-2])$""".r
  lazy val regexDashDashDashDashDay: Regex = """^----([0-2]\d|3[0-1])$""".r

  def nowEpochMicro: Long = Instant.now().toEpochMilli * 1000

  def expand_ymd_to_ymd_ym_y_md_m_d(ymd:String): Seq[String] = {
    Try(LocalDate.parse(ymd, DateTimeFormatterHolder.isoLocalDate)).toOption.map(localDate => Seq(
      localDate.toIsoLocalDateString,
      localDate.toYearDashMonthString,
      localDate.toYearString,
      localDate.toDashDashMonthDashDayString,
      localDate.toDashDashMonthString,
      localDate.toDashDashDashDashDayString
    )).getOrElse(Seq(ymd))
  }
}
