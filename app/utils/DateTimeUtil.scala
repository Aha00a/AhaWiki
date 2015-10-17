package utils

import java.time.Instant

object DateTimeUtil {
  def nowEpochNano: Long = Instant.now().toEpochMilli * 1000
}
