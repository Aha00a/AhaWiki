package com.aha00a.commons.utils

import org.apache.pekko.actor.ActorSystem
import play.api.Logging

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration

object SchedulerUtil extends Logging {
  def scheduleWithDynamicDelay(
    name: String,
    initialDelay: FiniteDuration,
    nextDelay: () => FiniteDuration,
    job: () => Unit
  )(
    implicit
    actorSystem: ActorSystem,
    executionContext: ExecutionContext
  ): Unit = {
    def scheduleOnce(delay: FiniteDuration): Unit = {
      logger.info(s"${FiniteDurationUtil.formatComma12dms(delay)}\tNext\t${name}")
      actorSystem.scheduler.scheduleOnce(delay) {
        StopWatch(name) {
          try {
            job.apply()
          } catch {
            case t: Throwable =>
              logger.error(s"SchedulerUtil.scheduleWithDynamicDelay\t${name}\tFailed\t${t}")
          }
        }
        scheduleOnce(nextDelay())
      }
    }

    scheduleOnce(initialDelay)
  }
}
