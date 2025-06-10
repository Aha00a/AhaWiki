package services

import actors.ActorAhaWiki.Calculate
import akka.actor.ActorRef
import akka.actor.ActorSystem
import com.aha00a.commons.Implicits.RichSeq
import com.aha00a.commons.utils.StopWatch
import logics.AhaWikiCache
import logics.ApplicationConf
import logics.SiteLogic
import models.tables.Site
import play.api.Environment
import play.api.Logging
import play.api.db.Database
import play.api.inject.ApplicationLifecycle
import play.api.libs.ws.WSClient
import play.api.mvc.ControllerComponents

import javax.inject._
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.Random

class ApplicationLifecycleHook @Inject()(
  implicit
  applicationLifecycle: ApplicationLifecycle,
  controllerComponents: ControllerComponents,
  actorSystem: ActorSystem,
  database: Database,
  environment: Environment,
  @Named("db-actor") actorAhaWiki: ActorRef,
  applicationConf: ApplicationConf,
  ahaWikiCache: AhaWikiCache,
  wsClient: WSClient,
  executionContext: ExecutionContext
) extends Logging {
  logger.info("OnApplicationStarting")

  applicationLifecycle.addStopHook { () =>
    logger.info("OnApplicationStop")
    Future.successful(())
  }

  def scheduleWithRandomInterval(name: String, min: Int, max: Int, f: () => Unit): Unit = {
    val delay = Random.between(min, max)
    logger.info(s"Schedule\t$name\tmin\t${min}s\tmax\t${max}s\tdelay\t${delay}s")
    actorSystem.scheduler.scheduleOnce(delay seconds) {
      f();
      scheduleWithRandomInterval(name, min, max, f)
    }
  }

  scheduleWithRandomInterval("deleteExpired", 60 * 1, 60 * 10, () => {
    StopWatch("deleteExpired") {
      database.withConnection { implicit connection =>
        val deletedRowCount = models.tables.AccessLog.deleteExpired()
        logger.info(s"""models.tables.AccessLog.deleteExpired()\tdeletedRowCount\t$deletedRowCount""")
      }
      database.withConnection { implicit connection =>
        val deletedRowCount = models.tables.IpDeny.deleteExpired()
        logger.info(s"""models.tables.IpDeny.deleteExpired()\tdeletedRowCount\t$deletedRowCount""")
      }
    }
  })

  scheduleWithRandomInterval("Calculate", 60, 60 * 20, () => {
    val site = SiteLogic.selectRandom()
    implicit val tupleDatabaseSite: (Database, Site) = (database, site)
    val count = 50
    val seq = ahaWikiCache.Page.SeqPageWithoutContentWithSizeLatest.get().shuffle().take(count)
    seq.zipWithIndex.foreach { case (page, i) =>
      actorAhaWiki ! Calculate(site, page.name, i, seq.length)
    }
  })

  logger.info("OnApplicationStarted")
}
