package com.aha00a.commons.utils

import java.time.format.DateTimeFormatter

object DateTimeFormatterHolder {
  lazy val isoLocalDateTimeWithMillisec: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSS")
  lazy val isoLocalDateTime: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss")

  lazy val isoLocalDate: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd")

  lazy val yearDashMonth: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM")
  lazy val year: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyy")
  lazy val monthDashDay: DateTimeFormatter = DateTimeFormatter.ofPattern("MM-dd")
  lazy val dashDashMonth: DateTimeFormatter = DateTimeFormatter.ofPattern("--MM")
  lazy val dashDashDashDashDay: DateTimeFormatter = DateTimeFormatter.ofPattern("----dd")

  lazy val isoLocalTime: DateTimeFormatter = DateTimeFormatter.ofPattern("HH:mm:ss.SSS")

}
