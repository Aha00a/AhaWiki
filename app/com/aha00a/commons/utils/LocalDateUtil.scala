package com.aha00a.commons.utils

import java.time.LocalDate
import scala.util.Try

object LocalDateUtil {
  def tryParse(s: String): Option[LocalDate] = {
    None
      .orElse(Try(LocalDate.parse(s, DateTimeFormatterHolder.isoLocalDate)).toOption)
      .orElse(Try(LocalDate.parse(s, DateTimeFormatterHolder.yearDashMonth)).toOption)
      .orElse(Try(LocalDate.parse(s, DateTimeFormatterHolder.year)).toOption)
  }
}
