package utils

import java.time.{Duration, LocalDateTime}

import play.api.Logger

object StopWatch {
  def apply[T](name:String)(operation: (String) => T): T = {
    val now = LocalDateTime.now()
    try {
      Logger.info(s"StopWatch - Start - $name")
      operation(name)
    } finally {
      Logger.info(s"StopWatch - Done  - $name - ${Duration.between(LocalDateTime.now(), now)}")
    }
  }
}
