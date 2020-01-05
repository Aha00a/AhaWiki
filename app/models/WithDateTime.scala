package models

import java.time.{LocalDate, LocalDateTime}
import java.util.Date

import com.aha00a.commons.Implicits.{LocalDateTimeFormatter, _}
import com.aha00a.commons.utils.DateTimeFormatterHolder


trait WithDateTime {
  val dateTime:Date

  lazy val localDateTime: LocalDateTime = dateTime.toLocalDateTime
  lazy val localDate: LocalDate = localDateTime.toLocalDate
  lazy val year: Int = localDate.getYear
  lazy val yearDashMonth: String = localDate.format(DateTimeFormatterHolder.yearDashMonth)

  lazy val toIsoLocalDateTimeString: String = localDateTime.toIsoLocalDateTimeString
  lazy val toIsoLocalDateString: String = localDateTime.toIsoLocalDateString
}
