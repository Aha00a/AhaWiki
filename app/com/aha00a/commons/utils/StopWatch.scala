package com.aha00a.commons.utils

import play.api.Logging

import java.time.Duration
import java.time.LocalDateTime

object StopWatch extends Logging {
  def apply[T](name:String, logWhenStart:Boolean = false)(operation: => T): T = {
    val now = LocalDateTime.now()
    try {
      if (logWhenStart) {
        logger.info(s"        \t$name\tStarted")
      }
      operation
    } finally {
      val duration = Duration.between(now, LocalDateTime.now())
      // TODO duration formatting - left padding with space
      logger.info(f"${duration.toMillis}%,8dms\t$name\tDone")
    }
  }
}
