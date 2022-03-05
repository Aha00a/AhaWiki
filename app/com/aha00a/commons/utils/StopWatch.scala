package com.aha00a.commons.utils

import java.time.{Duration, LocalDateTime}

import play.api.Logger
import play.api.Logging

object StopWatch extends Logging {
  def apply[T](name:String)(operation: => T): T = {
    val now = LocalDateTime.now()
    try {
      logger.info(s"$name\tStarted")
      operation
    } finally {
      logger.info(s"$name\tDone - ${Duration.between(LocalDateTime.now(), now)}")
    }
  }
}
