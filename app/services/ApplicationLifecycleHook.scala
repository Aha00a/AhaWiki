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
import javax.inject._
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.concurrent.duration.FiniteDuration
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
  executionContext: ExecutionContext,
  cacheFileCleanupService: CacheFileCleanupService
) extends Logging {
  logger.info("OnApplicationStarting")

  applicationLifecycle.addStopHook { () =>
    logger.info("OnApplicationStop")
    Future.successful(())
  }

  private def durationToMillisLong(duration: FiniteDuration): Long = duration.toMillis

  private def randomDelay(min: FiniteDuration, max: FiniteDuration): FiniteDuration = {
    val minMillis = durationToMillisLong(min)
    val maxMillis = durationToMillisLong(max)
    val delayMillis = Random.between(minMillis, maxMillis)
    delayMillis.millis
  }

  def registerScheduler(name: String, initialDelay: FiniteDuration, nextDelay: () => FiniteDuration, job: () => Unit): Unit = {
    def formatDelay(delay: FiniteDuration): String =
      if (delay >= 10.minutes) f"${java.time.Duration.ofMillis(delay.toMillis).toString}%14s"
      else f"${delay.toMillis}%,12dms"

    def scheduleOnce(delay: FiniteDuration): Unit = {
      logger.info(s"${formatDelay(delay)}\tNext\t${name}")
      actorSystem.scheduler.scheduleOnce(delay) {
        StopWatch(s"$name") {
          try {
            job.apply()
          } catch {
            case t: Throwable =>
              logger.error(s"ApplicationLifecycleHook.runScheduler\t${name}\tFailed\t${t}")
          }
        }
        scheduleOnce(nextDelay())
      }
    }

    scheduleOnce(initialDelay)
  }

  // 만료된 데이터 삭제 스케쥴러: 10~30분 간격으로 AccessLog, IpDeny, UserViewHistory 테이블에서 만료된 레코드를 삭제합니다.
  registerScheduler("deleteExpired", randomDelay(10.minutes, 30.minutes), () => randomDelay(10.minutes, 30.minutes), () => {
    database.withConnection { implicit connection =>
      models.tables.AccessLog.deleteExpired()
      models.tables.IpDeny.deleteExpired()
      models.tables.UserViewHistory.deleteExpired()
    }
  })

  registerScheduler("Calculate", randomDelay(20 seconds, 1 minutes), () => randomDelay(1 minute, 30 minutes), () => {
    val site = SiteLogic.selectRandom()
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
        val seq = ahaWikiCache.PageMeta.SeqPageLatestSummary.get().shuffle().take(count)
        seq.zipWithIndex.foreach { case (page, i) =>
          actorAhaWiki ! Calculate(site, page.name, i, seq.length)
        }
      }
    }
  })


  // 사이트 도메인 메모리 캐시 갱신 스케쥴러: 1시간 간격으로 AhaWikiCacheMemoryDomainSite를 미리 갱신합니다.
  registerScheduler("SiteDomainCacheRefresh", 5.seconds, () => 1.hour, () => {
    AhaWikiCacheMemoryDomainSite.refresh()
  })

  // 캐시 파일 정리 스케쥴러: 서버 로컬 타임존 기준 매주 1회(기본 7일 간격) 만료된 캐시 파일을 정리합니다.
  registerScheduler("CacheFileCleanup", 15 seconds, () => 7.days, () => {
    cacheFileCleanupService.cleanupAllExpiredCaches()
  })

  // 크롤러 캐시 TTL 정리 스케쥴러: 12시간 간격으로 180일 초과 캐시를 삭제합니다.
  registerScheduler("CrawlerCacheCleanup", 10.seconds, () => 12.hours, () => {
    database.withConnection { implicit connection =>
      models.tables.CacheCrawler.deleteExpired()
    }
  })

  logger.info("OnApplicationStarted")
}
