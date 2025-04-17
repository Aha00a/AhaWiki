package com.aha00a.commons.utils

import play.api.Logging

import java.time.Duration
import java.time.LocalDateTime

object StopWatch extends Logging {
  def apply[T](name:String)(operation: => T): T = {
    val now = LocalDateTime.now()
    try {
      logger.info(s"$name\tStarted")
      operation
    } finally {
      logger.info(s"$name\tDone - ${Duration.between(now, LocalDateTime.now())}")
    }
  }
}
