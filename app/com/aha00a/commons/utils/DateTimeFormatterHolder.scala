package com.aha00a.commons.utils

import java.time.format.DateTimeFormatter

object DateTimeFormatterHolder {
  lazy val yearDashMonth: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM")
  lazy val isoLocalDateTime: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss")
  lazy val isoLocalDateTimeWithMillisec: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSS")
  lazy val isoLocalDate: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd")
  lazy val isoLocalTime: DateTimeFormatter = DateTimeFormatter.ofPattern("HH:mm:ss.SSS")
}
