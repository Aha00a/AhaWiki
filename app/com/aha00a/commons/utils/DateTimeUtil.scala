package com.aha00a.commons.utils

import java.time.{Instant, LocalDate}

import com.aha00a.commons.Implicits._

import scala.util.Try

object DateTimeUtil {
  def nowEpochMicro: Long = Instant.now().toEpochMilli * 1000

  def expand_ymd_to_ymd_ym_y_md_m_d(ymd:String): Seq[String] = {
    Try(LocalDate.parse(ymd, DateTimeFormatterHolder.isoLocalDate)).toOption.map(localDate => Seq(
      localDate.toIsoLocalDateString,
      localDate.toYearDashMonthString,
      localDate.toYearString,
      localDate.toMonthDashDayString,
      localDate.toDashDashMonthString,
      localDate.toDashDashDashDashDayString
    )).getOrElse(Seq(ymd))
  }
}
