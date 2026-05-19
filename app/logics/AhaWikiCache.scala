package logics

import com.aha00a.commons.utils.StopWatch
import logics.wikis.RenderingMode
import logics.wikis.interpreters.Interpreters
import models.ContextSite
import models.ContextWikiPage
import models.PageLatestSummary
import models.tables.Site
import play.api.Environment
import play.api.Logging
import play.api.Mode.Dev
import play.api.cache.SyncCacheApi
import play.api.db.Database
import zio.json._

import javax.inject.Inject
import javax.inject.Singleton
import scala.annotation.unused
import scala.concurrent.duration._
import scala.reflect.ClassTag
import java.util.concurrent.ConcurrentHashMap

@Singleton
class AhaWikiCache @Inject()(syncCacheApi: SyncCacheApi, environment: Environment) extends Logging {
  private val cacheKeyLocks = new ConcurrentHashMap[String, AnyRef]()
  private val staleEntries = new ConcurrentHashMap[String, CachedJson]()

  private case class CachedJson(value: String, cachedAtEpochMs: Long)

  private def withSingleFlight[R](cacheKey: String)(f: => R): R = {
    val lock = cacheKeyLocks.computeIfAbsent(cacheKey, _ => new AnyRef)
    lock.synchronized {
      try f
      finally cacheKeyLocks.remove(cacheKey, lock)
    }
  }

  trait CacheEntity[T, I] {
    //noinspection ScalaWeakerAccess
    val durationExpire: FiniteDuration = if (environment.mode == Dev) 1 minute else 5 minutes

    def key()(implicit i: I): String
    def keyDefault()(implicit @unused i: I): String = s"${getClass.getName}"

    def invalidate()(implicit i: I): Unit = {
      StopWatch(Seq("Cache", "Invalidate", key()).mkString("\t")) {
        syncCacheApi.remove(key())
      }
    }

    def get()(implicit i: I, @unused classTag: ClassTag[T], encoder: JsonEncoder[T], decoder: JsonDecoder[T]): T = {
      val cacheKey = key()

      val json: String = try {
        val cachedJson = withSingleFlight(cacheKey) {
          syncCacheApi.getOrElseUpdate(cacheKey, durationExpire) {
            wrapOrElse().toJson
          }
        }
        staleEntries.put(cacheKey, CachedJson(cachedJson, System.currentTimeMillis()))
        cachedJson
      } catch {
        case timeoutException: java.util.concurrent.TimeoutException =>
          Option(staleEntries.get(cacheKey)).filterNot(isStaleBackupExpired).map(_.value) match {
            case Some(staleJson) =>
              logger.warn(s"Cache\tTimeout\t${cacheKey}\tServing stale cache", timeoutException)
              staleJson
            case None =>
              logger.warn(s"Cache\tTimeout\t${cacheKey}\tNo stale cache, falling back to uncached value", timeoutException)
              val refreshedJson = withSingleFlight(cacheKey) {
                wrapOrElse().toJson
              }
              staleEntries.put(cacheKey, CachedJson(refreshedJson, System.currentTimeMillis()))
              refreshedJson
          }
      }


      json.fromJson[T] match {
        case Left(e) =>
          logger.error(s"Cache\tParse\t${key()}\terror=$e")
          throw new RuntimeException(s"Cache\tParse\t${key()}\terror=$e")
        case Right(t) =>
          t
      }
    }

    private def wrapOrElse()(implicit i: I): T = {
      StopWatch(Seq("Cache", "Miss", key()).mkString("\t")) {
        orElse()
      }
    }

    private def isStaleBackupExpired(entry: CachedJson): Boolean = {
      val maxStaleMs = math.max(durationExpire.toMillis * 6, 60000L)
      System.currentTimeMillis() - entry.cachedAtEpochMs > maxStaleMs
    }

    def orElse()(implicit i: I): T
  }

  trait CacheEntityWithContextSite[T] extends CacheEntity[T, ContextSite] {
    override def key()(implicit contextSite: ContextSite): String = s"${getClass.getSimpleName}:${contextSite.site}"
  }

  def invalidateSiteCaches()(implicit database: Database, site: Site, contextSite: ContextSite): Unit = {
    AhaWikiCacheMemoryDomainSite.invalidate()
    implicit val ds = (database, site)
    PageMeta.SeqPageLatestSummary.invalidate()
    PageMeta.SeqPageName.invalidate()
    Header.invalidate()
    Footer.invalidate()
    Config.invalidate()
  }

  object Header extends CacheEntityWithContextSite[String] {
    override val durationExpire: FiniteDuration = if (environment.mode == Dev) 1 minute else 1 hour

    override def orElse()(implicit contextSite: ContextSite): String = contextSite.database.withConnection { implicit connection =>
      implicit val context: ContextWikiPage = contextSite.toWikiContext(Seq(""), RenderingMode.Normal)
      implicit val site: Site = context.site
      removePartialEditDataAttrs(
        Interpreters.toHtmlString(
          models.tables.Page.selectLastRevision(".header").map(_.content).orElse(DefaultPageLogic.getOption(".header").toOption).getOrElse("")
        )
      )
    }
  }

  object Footer extends CacheEntityWithContextSite[String] {
    override val durationExpire: FiniteDuration = if (environment.mode == Dev) 1 minute else 1 hour

    override def orElse()(implicit contextSite: ContextSite): String = contextSite.database.withConnection { implicit connection =>
      implicit val context: ContextWikiPage = contextSite.toWikiContext(Seq(""), RenderingMode.Normal)
      implicit val site: Site = context.site
      removePartialEditDataAttrs(
        Interpreters.toHtmlString(
          models.tables.Page.selectLastRevision(".footer").map(_.content).orElse(DefaultPageLogic.getOption(".footer").toOption).getOrElse("")
        )
      )
    }
  }

  private def removePartialEditDataAttrs(html: String): String = {
    html
      .replaceAll("""\sdata-edit-link="[^"]*"""", "")
      .replaceAll("""\sdata-edit-title="[^"]*"""", "")
  }

  object Config extends CacheEntityWithContextSite[String] {
    override def orElse()(implicit contextSite: ContextSite): String = contextSite.database.withConnection { implicit connection =>
      implicit val site: Site = contextSite.site
      models.tables.Page.selectLastRevision(".config").map(_.content).getOrElse("")
    }
  }

  object UserProfileImageUrl extends CacheEntity[Option[String], (Database, Site, String)] {
    override val durationExpire: FiniteDuration = if (environment.mode == Dev) 1 minute else 10 minutes

    override def key()(implicit t3: (Database, Site, String)): String = s"${getClass.getName}:${t3._2}:${t3._3}"

    override def orElse()(implicit t3: (Database, Site, String)): Option[String] = t3._1.withConnection { implicit connection =>
      val (_, _, nickname) = t3
      models.tables.User.selectByNickname(nickname).flatMap(_.profileImageUrl)
    }
  }

  object PageMeta {
    object SeqPageLatestSummary extends CacheEntity[Seq[PageLatestSummary], (Database, Site)] {
      override def key()(implicit t2: (Database, Site)): String = s"${getClass.getName}:${t2._2}"
      override def orElse()(implicit t2: (Database, Site)): Seq[PageLatestSummary] = t2._1.withConnection { implicit connection =>
        implicit val (_, site: Site) = t2
        models.tables.PageMeta.selectSeqPageLatestSummary()
      }
    }

    object SeqPageName extends CacheEntity[Seq[String], (Database, Site)] {
      override def key()(implicit t2: (Database, Site)): String = s"${getClass.getName}:${t2._2}"
      override def orElse()(implicit t2: (Database, Site)): Seq[String] = t2._1.withConnection { implicit connection =>
        implicit val (_, site: Site) = t2
        models.tables.PageMeta.selectSeqName()
      }
    }
  }
}
