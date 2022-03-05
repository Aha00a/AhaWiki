package services

import java.io.File
import java.util.Date

import actors.ActorAhaWiki.Calculate
import actors.ActorAhaWiki.CalculateCosineSimilarity
import actors.ActorAhaWiki.CalculateLink
import akka.actor.ActorRef
import akka.actor.ActorSystem
import com.aha00a.commons.Implicits._
import com.aha00a.commons.utils.Using
import javax.inject._
import models.tables.Page
import play.api.Logging
import play.api.db.Database
import play.api.inject.ApplicationLifecycle

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.io.Codec
import models.tables.Site

class ApplicationLifecycleHook @Inject()(implicit
                                         applicationLifecycle: ApplicationLifecycle,
                                         actorSystem: ActorSystem,
                                         executionContext: ExecutionContext,
                                         database: Database,
                                         @Named("db-actor") actorAhaWiki: ActorRef
                                        ) extends Logging {
  logger.info("OnApplicationStarting")

  applicationLifecycle.addStopHook { () =>
    logger.info("OnApplicationStop")
    Future.successful(())
  }

  actorSystem.scheduler.scheduleWithFixedDelay(25 seconds, 1 minutes)(() => {
    database.withConnection { implicit connection =>
      Site.selectRandom() map { implicit site: Site =>
        Page.pageSelectNameWhereNoCosineSimilarity() map { s =>
          actorAhaWiki ! CalculateCosineSimilarity(site, s)
        }
      }
    }
  })

  actorSystem.scheduler.scheduleWithFixedDelay(15 seconds, 30 seconds)(() => {
    database.withConnection { implicit connection =>
      Site.selectRandom() map { implicit site: Site =>
        val seq: Seq[String] = Page.pageSelectNameWhereNoLinkSrc()
        for ((v, i) <- seq.zipWithIndex) {
          actorAhaWiki ! Calculate(site, v, i, seq.length)
        }
      }
    }
  })

  logger.info("OnApplicationStarted")
}
