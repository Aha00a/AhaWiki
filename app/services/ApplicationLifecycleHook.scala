package services

import actors.ActorPageCalculator.Calculate
import com.aha00a.commons.Implicits.RichSeq
import com.aha00a.commons.utils.FiniteDurationUtil.random
import com.aha00a.commons.utils.SchedulerUtil
import logics.AhaWikiCache
import logics.AhaWikiCacheMemoryApiLinks
import logics.AhaWikiCacheMemoryDomainSite
import logics.AhaWikiCacheMemoryPermission
import logics.AhaWikiCacheMemorySiteAdmin
import logics.ApplicationConf
import logics.SiteLogic
import models.tables.Site
import org.apache.pekko.actor.ActorRef
import org.apache.pekko.actor.ActorSystem
import play.api.Environment
import play.api.Logging
import play.api.db.Database
import play.api.inject.ApplicationLifecycle
import play.api.libs.ws.WSClient
import play.api.mvc.ControllerComponents

import javax.inject._
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration._

class ApplicationLifecycleHook @Inject()(
  implicit
  applicationLifecycle: ApplicationLifecycle,
  controllerComponents: ControllerComponents,
  actorSystem: ActorSystem,
  database: Database,
  environment: Environment,
  @Named("actor-page-calculator") actorPageCalculator: ActorRef,
  applicationConf: ApplicationConf,
  ahaWikiCache: AhaWikiCache,
  ahaWikiCacheMemoryApiLinks: AhaWikiCacheMemoryApiLinks,
  wsClient: WSClient,
  executionContext: ExecutionContext,
  cacheFileCleanupService: CacheFileCleanupService
) extends Logging {
  private val schedulerExecutionContext: ExecutionContext =
    actorSystem.dispatchers.lookup("ahawiki-scheduler-dispatcher")

  logger.info("OnApplicationStarting")

  applicationLifecycle.addStopHook { () =>
    logger.info("OnApplicationStop")
    Future.successful(())
  }

  def scheduleWithDynamicDelay(name: String, initialDelay: FiniteDuration, nextDelay: () => FiniteDuration, job: () => Unit): Unit = {
    SchedulerUtil.scheduleWithDynamicDelay(name, initialDelay, nextDelay, job)(actorSystem, schedulerExecutionContext)
  }

  // 만료된 데이터 삭제 스케쥴러: 10~30분 간격으로 AccessLog, IpDeny, UserViewHistory 테이블에서 만료된 레코드를 삭제합니다.
  //
  // 보관 기간은 각 테이블의 `Retention` 값이고, 고른 근거도 그 옆에 있다. 여기 옮겨 적지
  // 않는다 — 값을 두 곳에 적으면 한쪽만 고쳐지고, 어느 쪽이 진짜인지 알 수 없게 된다.
  // 시작할 때 실제 값을 한 줄 찍으므로 한눈에 보려면 로그를 보면 된다.
  //
  // 셋 다 오래된 순으로 limit 개만 보고 배치로 지우므로, 기간을 줄여도 한 번에 몰아서
  // 지우지 않는다. 수렴까지 몇 번의 실행이 걸린다. 자세한 것은 ExpiredRows 참고.
  logger.info(
    "deleteExpired retention: " + Seq(
      s"AccessLog=${models.tables.AccessLog.Retention}",
      s"IpDeny=${models.tables.IpDeny.Retention}",
      s"UserViewHistory=${models.tables.UserViewHistory.Retention}",
    ).mkString(", ")
  )
  scheduleWithDynamicDelay("deleteExpired", random(10.minutes, 30.minutes), () => random(10.minutes, 30.minutes), () => {
    database.withConnection { implicit connection =>
      models.tables.AccessLog.deleteExpired()
      models.tables.IpDeny.deleteExpired()
      models.tables.UserViewHistory.deleteExpired()
    }
  })

  scheduleWithDynamicDelay("Calculate", random(20 seconds, 1 minutes), () => random(1 minute, 1 hour), () => {
    val site = SiteLogic.selectRandom()
    implicit val tupleDatabaseSite: (Database, Site) = (database, site)

    database.withConnection { implicit connection =>
      implicit val implicitSite: Site = site
      val missingPageNames = models.tables.PageMeta.selectMissingPageNames(limit = 10)
      if (missingPageNames.nonEmpty) {
        missingPageNames.zipWithIndex.foreach { case (pageName, i) =>
          actorPageCalculator ! Calculate(site, pageName, i, missingPageNames.length)
        }
      } else {
        logger.info(s"\t\tCalculate: no missing page names")
        if(false) {
          val seq = ahaWikiCache.PageMeta.SeqPageLatestSummary.get().shuffle().take(2)
          seq.zipWithIndex.foreach { case (page, i) =>
            actorPageCalculator ! Calculate(site, page.name, i, seq.length)
          }
        }
      }
    }
  })


  // 사이트 도메인 메모리 캐시 갱신 스케쥴러: 1시간 간격으로 AhaWikiCacheMemoryDomainSite를 미리 갱신합니다.
  scheduleWithDynamicDelay("SiteDomainCacheRefresh", 5.seconds, () => 1.hour, () => {
    AhaWikiCacheMemoryDomainSite.refresh()
  })

  // API 링크 캐시 정리 스케쥴러: 10분 간격으로 AhaWikiCacheMemoryApiLinks를 초기화합니다.
  scheduleWithDynamicDelay("ApiLinksCacheClear", 10.minutes, () => 10.minutes, () => {
    ahaWikiCacheMemoryApiLinks.clear()
  })

  // Permission 캐시 정리 스케쥴러: 6시간 간격으로 AhaWikiCacheMemoryPermission을 초기화합니다.
  scheduleWithDynamicDelay("PermissionCacheClear", 6.hours, () => 6.hours, () => {
    AhaWikiCacheMemoryPermission.clear()
  })

  // SiteAdmin 캐시 정리 스케쥴러: 6시간 간격으로 AhaWikiCacheMemorySiteAdmin을 초기화합니다.
  scheduleWithDynamicDelay("SiteAdminCacheClear", 6.hours, () => 6.hours, () => {
    AhaWikiCacheMemorySiteAdmin.clear()
  })

  // 캐시 파일 정리 스케쥴러: 서버 로컬 타임존 기준 매주 1회(기본 7일 간격) 만료된 캐시 파일을 정리합니다.
  scheduleWithDynamicDelay("CacheFileCleanup", 15 seconds, () => 7.days, () => {
    cacheFileCleanupService.cleanupAllExpiredCaches()
  })

  // 크롤러 캐시 TTL 정리 스케쥴러: 12시간 간격으로 180일 초과 캐시를 삭제합니다.
  scheduleWithDynamicDelay("CrawlerCacheCleanup", 10.seconds, () => 12.hours, () => {
    database.withConnection { implicit connection =>
      models.tables.CacheCrawler.deleteExpired()
    }
  })

  logger.info("OnApplicationStarted")
}
