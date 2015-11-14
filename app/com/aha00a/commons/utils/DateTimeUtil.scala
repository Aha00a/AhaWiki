package com.aha00a.commons.utils

import java.time.Instant

object DateTimeUtil {
  def nowEpochNano: Long = Instant.now().toEpochMilli * 1000
}
