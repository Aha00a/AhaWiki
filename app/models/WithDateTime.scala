package models

import java.time.{LocalDate, LocalDateTime}
import java.util.Date

import com.aha00a.commons.Implicits.LocalDateTimeFormatter
import com.aha00a.commons.utils.{DateTimeFormatterHolder, LocalDateTimeUtil}


trait WithDateTime {
  val dateTime:Date

  lazy val localDateTime: LocalDateTime = LocalDateTimeUtil.convert(dateTime)
  lazy val localDate: LocalDate = localDateTime.toLocalDate
  lazy val year: Int = localDate.getYear
  lazy val yearDashMonth: String = localDate.format(DateTimeFormatterHolder.yearDashMonth)

  lazy val isoLocalDateTime: String = localDateTime.toIsoLocalDateTimeString
}
