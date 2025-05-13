package com.aha00a.commons

import java.time.Duration
import java.time.LocalDateTime
import java.time.Period

object PeriodDuration {
  def between(startDateInclusive: LocalDateTime, endDateExclusive: LocalDateTime): PeriodDuration = {
    val period: Period = Period.between(startDateInclusive.toLocalDate, endDateExclusive.toLocalDate)
    val localDateTimeIntermediate: LocalDateTime = startDateInclusive
      .plusYears(period.getYears)
      .plusMonths(period.getMonths)
      .plusDays(period.getDays)

    val duration: Duration = java.time.Duration.between(localDateTimeIntermediate, endDateExclusive)
    PeriodDuration(
      years = period.getYears,
      months = period.getMonths,
      days = period.getDays,
      hours = duration.toHours,
      minutes = duration.toMinutes % 60,
      seconds = duration.getSeconds % 60,
      milliseconds = duration.toMillis % 1000
    )
  }

  private val iso8601Pattern = """P(?:(\d+)Y)?(?:(\d+)M)?(?:(\d+)D)?(?:T(?:(\d+)H)?(?:(\d+)M)?(?:(\d+(?:\.\d+)?)S)?)?""".r

  def fromISO8601(s: String): PeriodDuration = s match {
    case iso8601Pattern(yStr, moStr, dStr, hStr, miStr, sStr) =>
      val years = Option(yStr).map(_.toInt).getOrElse(0)
      val months = Option(moStr).map(_.toInt).getOrElse(0)
      val days = Option(dStr).map(_.toInt).getOrElse(0)
      val hours = Option(hStr).map(_.toLong).getOrElse(0L)
      val minutes = Option(miStr).map(_.toLong).getOrElse(0L)

      val (seconds, milliseconds) = Option(sStr) match {
        case Some(value) =>
          val totalSeconds = value.toDouble
          val secs = totalSeconds.toLong
          val millis = ((totalSeconds - secs) * 1000).round
          (secs, millis)
        case None => (0L, 0L)
      }

      PeriodDuration(years, months, days, hours, minutes, seconds, milliseconds)

    case _ => throw new IllegalArgumentException(s"Invalid ISO8601 duration: $s")
  }

  def fromISO8601OrNone(s: String): Option[PeriodDuration] = scala.util.Try(fromISO8601(s)).toOption
}

case class PeriodDuration(
                           years: Int = 0,
                           months: Int = 0,
                           days: Int = 0,
                           hours: Long = 0,
                           minutes: Long = 0,
                           seconds: Long = 0,
                           milliseconds: Long = 0,
                         ) {
  def toPeriod: Period = Period.of(years, months, days)

  def toDuration: java.time.Duration = {
    java.time.Duration.ZERO
      .plusDays((years * 365.2422.toLong))
      .plusDays((months * 365 / 12.0).toLong)
      .plusDays(days)
      .plusHours(hours)
      .plusMinutes(minutes)
      .plusSeconds(seconds)
      .plusMillis(milliseconds)
  }

  def toISO8601: String = {
    def partForSecond(seconds: Long, millis: Long): String = {
      if (seconds != 0 || millis != 0) {
        val totalSeconds = seconds + millis / 1000.0
        f"$totalSeconds%.3fS".replaceAll("""\.?0+S$""", "S")
      } else ""
    }

    def partForOthers(value: Long, suffix: String): String = if (value != 0) s"$value$suffix" else ""

    val datePart = partForOthers(years, "Y") + partForOthers(months, "M") + partForOthers(days, "D")
    val timePart = partForOthers(hours, "H") + partForOthers(minutes, "M") + partForSecond(seconds, milliseconds)
    (datePart, timePart) match {
      case ("", "") => "PT0S"
      case (_, "") => s"P$datePart"
      case _ => s"P${datePart}T$timePart"
    }
  }

  def unary_- : PeriodDuration = this.negate

  def +(other: PeriodDuration): PeriodDuration = this.add(other)

  def -(other: PeriodDuration): PeriodDuration = this.minus(other)

  def negate: PeriodDuration = {
    PeriodDuration(
      years = -years,
      months = -months,
      days = -days,
      hours = -hours,
      minutes = -minutes,
      seconds = -seconds,
      milliseconds = -milliseconds
    )
  }

  def add(other: PeriodDuration): PeriodDuration = {
    val newMilliseconds = this.milliseconds + other.milliseconds
    val newSeconds = this.seconds + other.seconds
    val newMinutes = this.minutes + other.minutes
    val newHours = this.hours + other.hours
    val newDays = this.days + other.days
    val newMonths = this.months + other.months
    val newYears = this.years + other.years

    PeriodDuration(
      years = newYears,
      months = newMonths,
      days = newDays,
      hours = newHours,
      minutes = newMinutes,
      seconds = newSeconds,
      milliseconds = newMilliseconds
    ).normalize
  }

  def minus(other: PeriodDuration): PeriodDuration = {
    this.add(other.negate)
  }

  def normalize: PeriodDuration = {
    def normalizeUnit(value: Long, threshold: Long, normalizeFn: Long => Long): (Long, Long) = {
      if (value >= threshold) {
        val normalizedValue = value / threshold
        (normalizeFn(normalizedValue), value % threshold)
      } else if (value < 0) {
        val normalizedValue = normalizeFn(value)
        (normalizedValue, value + threshold)
      } else {
        (0L, value)
      }
    }

    val (normalizedYears, remainingMonths) = normalizeUnit(years, 12, identity)
    val (normalizedMonths, remainingDays) = normalizeUnit(remainingMonths, 30, identity)
    val (normalizedDays, remainingHours) = normalizeUnit(remainingDays, 24, identity)
    val (normalizedHours, remainingMinutes) = normalizeUnit(remainingHours, 60, identity)
    val (normalizedMinutes, remainingSeconds) = normalizeUnit(remainingMinutes, 60, identity)
    val (normalizedSeconds, remainingMillis) = normalizeUnit(remainingSeconds, 1000, identity)

    PeriodDuration(
      years = normalizedYears.toInt,
      months = normalizedMonths.toInt,
      days = normalizedDays.toInt,
      hours = normalizedHours,
      minutes = normalizedMinutes,
      seconds = normalizedSeconds,
      milliseconds = remainingMillis
    )
  }
}
