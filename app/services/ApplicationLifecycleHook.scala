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

import java.time.Instant
import java.util.concurrent.ConcurrentHashMap
import javax.inject._
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.Random

case class SchedulerStatus(
  name: String,
  minSeconds: Int,
  maxSeconds: Int,
  running: Boolean,
  nextDelaySeconds: Option[Int],
  lastStartedAt: Option[String],
  lastFinishedAt: Option[String],
  lastResult: Option[String],
  runCount: Long,
) {
  def withRunning(nextDelay: Option[Int]): SchedulerStatus = {
    copy(running = true, nextDelaySeconds = nextDelay, lastStartedAt = Some(Instant.now().toString), runCount = runCount + 1)
  }

  def withCompleted(result: String): SchedulerStatus = {
    copy(running = false, lastFinishedAt = Some(Instant.now().toString), lastResult = Some(result))
  }
}

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

  private val schedulerMap = new ConcurrentHashMap[String, SchedulerStatus]()
  private val jobMap = new ConcurrentHashMap[String, () => Unit]()

  applicationLifecycle.addStopHook { () =>
    logger.info("OnApplicationStop")
    Future.successful(())
  }

  private def withSchedulerStatus(name: String)(f: SchedulerStatus => SchedulerStatus): Unit = {
    val current = Option(schedulerMap.get(name)).getOrElse {
      throw new IllegalStateException(s"Unknown scheduler: $name")
    }
    schedulerMap.put(name, f(current))
  }

  private def runScheduler(name: String): Unit = {
    val maybeJob = Option(jobMap.get(name))
    if (maybeJob.isEmpty) {
      logger.warn(s"Unknown scheduler run requested: $name")
      return
    }

    val schedulerStatus = Option(schedulerMap.get(name))
    if (schedulerStatus.exists(_.running)) {
      logger.info(s"Skip scheduler run because already running: $name")
      return
    }

    withSchedulerStatus(name)(_.withRunning(None))
    try {
      maybeJob.get.apply()
      withSchedulerStatus(name)(_.withCompleted("ok"))
    } catch {
      case t: Throwable =>
        logger.error(s"Scheduler execution failed: $name", t)
        withSchedulerStatus(name)(_.withCompleted(s"error: ${t.getClass.getSimpleName}"))
    }
  }

  private def registerScheduler(name: String, min: Int, max: Int, job: () => Unit): Unit = {
    jobMap.put(name, job)
    schedulerMap.put(name, SchedulerStatus(name, min, max, running = false, nextDelaySeconds = None, lastStartedAt = None, lastFinishedAt = None, lastResult = None, runCount = 0))

    def scheduleWithRandomInterval(): Unit = {
      val delay = Random.between(min, max)
      logger.info(s"Schedule\t$name\tmin\t${min}s\tmax\t${max}s\tdelay\t${delay}s")
      withSchedulerStatus(name)(_.copy(nextDelaySeconds = Some(delay)))
      actorSystem.scheduler.scheduleOnce(delay seconds) {
        runScheduler(name)
        scheduleWithRandomInterval()
      }
    }

    scheduleWithRandomInterval()
  }

  def getSchedulerStatuses: Seq[SchedulerStatus] = {
    import scala.jdk.CollectionConverters._
    schedulerMap.values().asScala.toSeq.sortBy(_.name)
  }

  def runSchedulerNow(name: String): Boolean = {
    if (!jobMap.containsKey(name)) {
      false
    } else {
      actorSystem.scheduler.scheduleOnce(0.seconds) {
        runScheduler(name)
      }
      true
    }
  }

  // 만료된 데이터 삭제 스케쥴러: 10~30분 간격으로 AccessLog와 IpDeny 테이블에서 만료된 레코드를 삭제합니다.
  registerScheduler("deleteExpired", 60 * 10, 60 * 30, () => {
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

  // 페이지 계산 스케쥴러: 20~60분 간격으로 랜덤 사이트를 선택하여 랜덤 페이지 10개에 대해 계산 작업을 ActorAhaWiki에 요청합니다.
  registerScheduler("Calculate", 60 * 20, 60 * 60, () => {
    val site = SiteLogic.selectRandom()
    implicit val tupleDatabaseSite: (Database, Site) = (database, site)
    val count = 10
    val seq = ahaWikiCache.Page.SeqPageWithoutContentWithSizeLatest.get().shuffle().take(count)
    seq.zipWithIndex.foreach { case (page, i) =>
      actorAhaWiki ! Calculate(site, page.name, i, seq.length)
    }
  })

  logger.info("OnApplicationStarted")
}
