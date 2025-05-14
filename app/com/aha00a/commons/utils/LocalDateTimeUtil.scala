package com.aha00a.commons.utils

import java.time.Instant
import java.time.LocalDate
import java.time.LocalDateTime
import java.time.ZoneId
import java.util.Date
import scala.util.Try

object LocalDateTimeUtil {
  def fromEpochSecond(time:Long): LocalDateTime = LocalDateTime.ofInstant(Instant.ofEpochSecond(time), ZoneId.systemDefault())
  def fromEpochMilli(time:Long): LocalDateTime = LocalDateTime.ofInstant(Instant.ofEpochMilli(time), ZoneId.systemDefault())
  def fromEpochMicro(time:Long): LocalDateTime = fromEpochMilli(time / 1000)
  def convert(date: Date): LocalDateTime = LocalDateTime.ofInstant(date.toInstant, ZoneId.systemDefault())

  def tryParse(s: String): Option[LocalDateTime] = {
    None
      .orElse(Try(LocalDateTime.parse(s, DateTimeFormatterHolder.isoLocalDateTimeWithMillisec)).toOption)  // 밀리초까지 처리
      .orElse(Try(LocalDateTime.parse(s, DateTimeFormatterHolder.isoLocalDateTime)).toOption)  // 일반 LocalDateTime 포맷
      .orElse(Try(LocalDate.parse(s, DateTimeFormatterHolder.isoLocalDate)).toOption.map(_.atStartOfDay()))  // 날짜만 있는 경우, 00:00:00 추가
      .orElse(Try(LocalDate.parse(s, DateTimeFormatterHolder.yearDashMonth)).toOption.map(_.atStartOfDay()))  // 연-월 포맷 처리
      .orElse(Try(LocalDate.parse(s, DateTimeFormatterHolder.year)).toOption.map(_.atStartOfDay()))  // 연도만 있는 경우, 00:00:00 추가
  }

  def tryParseIso(s: String): Option[LocalDateTime] = Try(LocalDateTime.parse(s, DateTimeFormatterHolder.isoLocalDateTime)).toOption
}
