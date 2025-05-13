package com.aha00a.commons.utils

import java.time.Instant
import java.time.LocalDateTime
import java.time.ZoneId
import java.util.Date
import scala.util.Try

object LocalDateTimeUtil {
  def fromEpochSecond(time:Long): LocalDateTime = LocalDateTime.ofInstant(Instant.ofEpochSecond(time), ZoneId.systemDefault())
  def fromEpochMilli(time:Long): LocalDateTime = LocalDateTime.ofInstant(Instant.ofEpochMilli(time), ZoneId.systemDefault())
  def fromEpochMicro(time:Long): LocalDateTime = fromEpochMilli(time / 1000)
  def convert(date: Date): LocalDateTime = LocalDateTime.ofInstant(date.toInstant, ZoneId.systemDefault())

  def tryParse(s: String): Option[LocalDateTime] = Try(LocalDateTime.parse(s)).toOption
  def tryParseIso(s: String): Option[LocalDateTime] = Try(LocalDateTime.parse(s, DateTimeFormatterHolder.isoLocalDateTime)).toOption
}
