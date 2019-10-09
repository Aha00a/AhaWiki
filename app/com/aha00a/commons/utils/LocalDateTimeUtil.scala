package com.aha00a.commons.utils

import java.time.{Instant, LocalDateTime, ZoneId}
import java.util.Date

object LocalDateTimeUtil {
  def fromEpochSecond(time:Long): LocalDateTime = LocalDateTime.ofInstant(Instant.ofEpochSecond(time), ZoneId.systemDefault())
  def fromEpochMilli(time:Long): LocalDateTime = LocalDateTime.ofInstant(Instant.ofEpochMilli(time), ZoneId.systemDefault())
  def fromEpochNano(time:Long): LocalDateTime = fromEpochMilli(time / 1000)
  def convert(date: Date): LocalDateTime = LocalDateTime.ofInstant(date.toInstant, ZoneId.systemDefault())
}
