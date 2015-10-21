package utils

import java.time.{Duration, LocalDateTime}

import play.api.Logger

object StopWatch {
  def apply[T](name:String)(operation: => T): T = {
    val now = LocalDateTime.now()
    try {
      Logger.info(s"StopWatch - Start - $name")
      operation
    } finally {
      Logger.info(s"StopWatch - Done  - $name - ${Duration.between(LocalDateTime.now(), now)}")
    }
  }
}
