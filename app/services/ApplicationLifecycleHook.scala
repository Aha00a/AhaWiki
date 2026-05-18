package services

import actors.ActorAhaWiki.Calculate
import akka.actor.ActorRef
import akka.actor.ActorSystem
import com.aha00a.commons.Implicits.RichSeq
import com.aha00a.commons.utils.StopWatch
import logics.AhaWikiCache
import logics.AhaWikiCacheMemoryDomainSite
import logics.ApplicationConf
import logics.SiteLogic
import models.tables.Site
import play.api.Environment
import play.api.Logging
import play.api.db.Database
import play.api.inject.ApplicationLifecycle
import play.api.libs.ws.WSClient
import play.api.mvc.ControllerComponents

import java.time.{Duration, Instant, LocalTime, ZoneId, ZonedDateTime}
import java.util.concurrent.ConcurrentHashMap
import javax.inject._
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.concurrent.duration.FiniteDuration
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
  executionContext: ExecutionContext,
  cacheFileCleanupService: CacheFileCleanupService
) extends Logging {
  logger.info("OnApplicationStarting")

  private val schedulerMap = new ConcurrentHashMap[String, SchedulerStatus]()
  private val jobMap = new ConcurrentHashMap[String, () => Unit]()

  private def logScheduler(name: String, event: String, details: (String, Any)*): Unit = {
    val suffix = if (details.nonEmpty) details.map { case (k, v) => s"$k=$v" }.mkString(" ") else ""
    logger.info(s"Scheduler\t$name\t$event${if (suffix.nonEmpty) s"\t$suffix" else ""}")
  }

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
    StopWatch(s"$name") {
      val maybeJob = Option(jobMap.get(name))
      if (maybeJob.isEmpty) {
        logScheduler(name, "run-skipped", "reason" -> "unknown-scheduler")
        return
      }

      val schedulerStatus = Option(schedulerMap.get(name))
      if (schedulerStatus.exists(_.running)) {
        logScheduler(name, "run-skipped", "reason" -> "already-running")
        return
      }

      withSchedulerStatus(name)(_.withRunning(None))
      val startedAtNanos = System.nanoTime()
      try {
        maybeJob.get.apply()
        withSchedulerStatus(name)(_.withCompleted("ok"))
      } catch {
        case t: Throwable =>
          logger.error(s"ApplicationLifecycleHook.runScheduler\t$name\tFailed\t$t")
          withSchedulerStatus(name)(_.withCompleted(s"error: ${t.getClass.getSimpleName}"))
      }
    }
  }


  private def durationToSecondsInt(duration: FiniteDuration): Int = (duration.toMillis / 1000L).toInt

  private def durationToMillisLong(duration: FiniteDuration): Long = duration.toMillis

  private def registerScheduler(name: String, min: FiniteDuration, max: FiniteDuration, job: () => Unit): Unit = {
    jobMap.put(name, job)
    schedulerMap.put(name, SchedulerStatus(name, durationToSecondsInt(min), durationToSecondsInt(max), running = false, nextDelaySeconds = None, lastStartedAt = None, lastFinishedAt = None, lastResult = None, runCount = 0))

    def scheduleWithRandomInterval(): Unit = {
      val minMillis = durationToMillisLong(min)
      val maxMillis = durationToMillisLong(max)
      val delayMillis = Random.between(minMillis, maxMillis)
      val delay = delayMillis.millis
      logScheduler(name, "next-run", "strategy" -> "random-interval", "minSeconds" -> durationToSecondsInt(min), "maxSeconds" -> durationToSecondsInt(max), "delaySeconds" -> durationToSecondsInt(delay))
      withSchedulerStatus(name)(_.copy(nextDelaySeconds = Some(durationToSecondsInt(delay))))
      actorSystem.scheduler.scheduleOnce(delay) {
        runScheduler(name)
        scheduleWithRandomInterval()
      }
    }

    scheduleWithRandomInterval()
  }


  def registerFixedDelayScheduler(name: String, initialDelay: FiniteDuration, interval: FiniteDuration, job: () => Unit): Unit = {
    val min = durationToSecondsInt(interval)
    val max = durationToSecondsInt(interval)
    jobMap.put(name, job)
    schedulerMap.put(name, SchedulerStatus(name, min, max, running = false, nextDelaySeconds = None, lastStartedAt = None, lastFinishedAt = None, lastResult = None, runCount = 0))

    def scheduleOnce(delay: FiniteDuration): Unit = {
      withSchedulerStatus(name)(_.copy(nextDelaySeconds = Some(durationToSecondsInt(delay))))
      logScheduler(name, "next-run", "strategy" -> "fixed-delay", "delaySeconds" -> durationToSecondsInt(delay))
      actorSystem.scheduler.scheduleOnce(delay) {
        runScheduler(name)
        scheduleOnce(interval)
      }
    }

    scheduleOnce(initialDelay)
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

  // 만료된 데이터 삭제 스케쥴러: 10~30분 간격으로 AccessLog, IpDeny, UserViewHistory 테이블에서 만료된 레코드를 삭제합니다.
  registerScheduler("deleteExpired", 10.minutes, 30.minutes, () => {
    StopWatch("deleteExpired") {
      database.withConnection { implicit connection =>
        val deletedRowCount = models.tables.AccessLog.deleteExpired()
        logger.info(s"""models.tables.AccessLog.deleteExpired()\tdeletedRowCount\t$deletedRowCount""")
      }
      database.withConnection { implicit connection =>
        val deletedRowCount = models.tables.IpDeny.deleteExpired()
        logger.info(s"""models.tables.IpDeny.deleteExpired()\tdeletedRowCount\t$deletedRowCount""")
      }
      database.withConnection { implicit connection =>
        val deletedRowCount = models.tables.UserViewHistory.deleteExpired()
        logger.info(s"""models.tables.UserViewHistory.deleteExpired()\tdeletedRowCount\t$deletedRowCount""")
      }
    }
  })

  registerScheduler("Calculate", 20 seconds, 30 seconds, () => {
    val site = if(false) SiteLogic.selectRandom() else SiteLogic.get(1L).getOrElse(Site.notFound) // TODO
    implicit val tupleDatabaseSite: (Database, Site) = (database, site)
    val count = 10

    database.withConnection { implicit connection =>
      implicit val implicitSite: Site = site
      val missingPageNames = models.tables.PageMeta.selectMissingPageNames(limit = count)
      if (missingPageNames.nonEmpty) {
        missingPageNames.zipWithIndex.foreach { case (pageName, i) =>
          actorAhaWiki ! Calculate(site, pageName, i, missingPageNames.length)
        }
      } else {
        logger.info(s"Calculate: no missing page names")
        val seq = ahaWikiCache.Page.SeqPageWithoutContentWithSizeLatest.get().shuffle().take(count)
        seq.zipWithIndex.foreach { case (page, i) =>
          actorAhaWiki ! Calculate(site, page.name, i, seq.length)
        }
      }
    }
  })


  // 사이트 도메인 메모리 캐시 갱신 스케쥴러: 1시간 간격으로 AhaWikiCacheMemoryDomainSite를 미리 갱신합니다.
  registerFixedDelayScheduler("SiteDomainCacheRefresh", 5.seconds, 1.hour, () => {
    StopWatch("SiteDomainCacheRefresh") {
      AhaWikiCacheMemoryDomainSite.refresh()
    }
  })

  // 캐시 파일 정리 스케쥴러: 서버 로컬 타임존 기준 매주 1회(기본 7일 간격) 만료된 캐시 파일을 정리합니다.
  registerFixedDelayScheduler("CacheFileCleanup", 15 seconds, 7.days, () => {
    StopWatch("CacheFileCleanup") {
      val results = cacheFileCleanupService.cleanupAllExpiredCaches()
      results.foreach { result =>
        logger.info(s"CacheFileCleanup${result.name}deletedCount${result.deletedCount}")
      }
    }
  })

  // 크롤러 캐시 TTL 정리 스케쥴러: 12시간 간격으로 180일 초과 캐시를 삭제합니다.
  registerFixedDelayScheduler("CrawlerCacheCleanup", 10.seconds, 12.hours, () => {
    StopWatch("CrawlerCacheCleanup") {
      database.withConnection { implicit connection =>
        val deletedRowCount = models.tables.CacheCrawler.deleteExpired()
        logger.info(s"""models.tables.CacheCrawler.deleteExpired()\tdeletedRowCount\t$deletedRowCount""")
      }
    }
  })

  logger.info("OnApplicationStarted")
}
