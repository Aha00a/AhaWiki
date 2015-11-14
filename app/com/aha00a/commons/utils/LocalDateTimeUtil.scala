package com.aha00a.commons.utils

import java.time.{Instant, LocalDateTime, ZoneId}

object LocalDateTimeUtil {
  def fromEpochSecond(time:Long) = LocalDateTime.ofInstant(Instant.ofEpochSecond(time), ZoneId.systemDefault())
  def fromEpochMilli(time:Long) = LocalDateTime.ofInstant(Instant.ofEpochMilli(time), ZoneId.systemDefault())
  def fromEpochNano(time:Long) = fromEpochMilli(time / 1000)
}
