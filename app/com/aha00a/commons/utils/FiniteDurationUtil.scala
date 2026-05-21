package com.aha00a.commons.utils

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration._
import scala.util.Random

object FiniteDurationUtil {
  def formatComma12dms(delay: FiniteDuration): String = {
    if (delay < 10.minutes) {
      f"${delay.toMillis}%,12dms"
    } else {
      f"${java.time.Duration.ofMillis(delay.toMillis).toString}%14s"
    }
  }

  def random(durationMinimum: FiniteDuration, durationMaximum: FiniteDuration): FiniteDuration = {
    Random.between(durationMinimum.toMillis, durationMaximum.toMillis).millis
  }
}
