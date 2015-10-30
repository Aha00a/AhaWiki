package utils

import java.time.format.DateTimeFormatter

object DateTimeFormatterHolder {
  lazy val yearDashMonth = DateTimeFormatter.ofPattern("yyyy-MM")
  lazy val isoDateTime = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSS")
  lazy val isoDate = DateTimeFormatter.ofPattern("yyyy-MM-dd")
  lazy val isoTime = DateTimeFormatter.ofPattern("HH:mm:ss.SSS")
}
