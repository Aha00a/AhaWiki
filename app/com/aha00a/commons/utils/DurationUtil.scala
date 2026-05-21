package com.aha00a.commons.utils

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration._
import scala.util.Random

object DurationUtil {
  def random(durationMinimum: FiniteDuration, durationMaximum: FiniteDuration): FiniteDuration = {
    Random.between(durationMinimum.toMillis, durationMaximum.toMillis).millis
  }
}
