package logics

import com.aha00a.commons.utils.FiniteDurationUtil
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

/**
 * Values shared by the instances through Redis, each kept decoded in this process as well.
 *
 * Next to each value Redis holds a short version key, rewritten whenever the value is. A read asks
 * Redis for the version only, and when it matches the one this process decoded, returns that
 * decoded value without fetching or decoding the value itself. Before this, every read fetched
 * and decoded the whole JSON: the page list of the largest site is over a megabyte, and a page
 * view on any other site read it to look for a twin page.
 *
 * Asking Redis keeps it exactly as fresh as reading the value was: an invalidation on either
 * instance removes the version, and the next read anywhere fetches again. Decoded values are
 * shared between callers, so every cached type must be immutable.
 */
@Singleton
class AhaWikiCache @Inject()(syncCacheApi: SyncCacheApi, environment: Environment) extends Logging {
  private val cacheKeyLocks = new ConcurrentHashMap[String, AnyRef]()
  private val decodedEntries = new ConcurrentHashMap[String, Decoded]()

  /**
   * A value as this process last decoded it. `version` is the version key read before the value
   * was, and None when there was none -- a value written before version keys existed, or one
   * whose version was removed in between -- so it never matches and the next read fetches again.
   * It is still the fallback when Redis fails, for as long as `isStaleBackupExpired` allows,
   * counted from `confirmedAtEpochMs`: the last time Redis said it was current.
   */
  private class Decoded(val version: Option[String], val value: Any) {
    @volatile var confirmedAtEpochMs: Long = System.currentTimeMillis()
  }

  private val VersionKeySuffix = ":version"

  private val decodedMaxMs: Long = 12 * 3600 * 1000L  // drop entries not confirmed for 12 hours
  private val decodedMaxEntries: Int = 1000

  private def cleanupDecodedEntries(): Unit = {
    val cutoff = System.currentTimeMillis() - decodedMaxMs
    val iter   = decodedEntries.entrySet().iterator()
    while (iter.hasNext) {
      if (iter.next().getValue.confirmedAtEpochMs < cutoff) iter.remove()
    }
  }

  private def rememberDecoded(cacheKey: String, decoded: Decoded): Unit = {
    decodedEntries.put(cacheKey, decoded)
    if (decodedEntries.size() > decodedMaxEntries) {
      cleanupDecodedEntries()
      val iter = decodedEntries.entrySet().iterator()
      while (decodedEntries.size() > decodedMaxEntries && iter.hasNext) {
        iter.next()
        iter.remove()
      }
    }
  }

  private def withSingleFlight[R](cacheKey: String)(f: => R): R = {
    val lock = cacheKeyLocks.computeIfAbsent(cacheKey, _ => new AnyRef)
    lock.synchronized {
      try f
      finally cacheKeyLocks.remove(cacheKey, lock)
    }
  }

  trait CacheEntity[T, I] {
    //noinspection ScalaWeakerAccess
    def durationExpire: FiniteDuration = if (environment.mode == Dev) 1 minute else 5 minutes

    def key()(implicit i: I): String
    def keyDefault()(implicit @unused i: I): String = s"${getClass.getName}"

    def invalidate()(implicit i: I): Unit = {
      val cacheKey = key()
      StopWatch(Seq("Cache", "Invalidate", cacheKey).mkString("\t")) {
        syncCacheApi.remove(cacheKey + VersionKeySuffix)
        syncCacheApi.remove(cacheKey)
        decodedEntries.remove(cacheKey)
      }
    }

    def get()(implicit i: I, @unused classTag: ClassTag[T], encoder: JsonEncoder[T], decoder: JsonDecoder[T]): T = {
      val cacheKey = key()

      if (scala.util.Random.nextInt(500) == 0) cleanupDecodedEntries()

      try {
        currentDecoded(cacheKey, syncCacheApi.get[String](cacheKey + VersionKeySuffix))
          .getOrElse(withSingleFlight(cacheKey)(fetch(cacheKey)))
          .value.asInstanceOf[T]
      } catch {
        case e: Exception =>
          Option(decodedEntries.get(cacheKey)).filterNot(isStaleBackupExpired) match {
            case Some(stale) =>
              logger.warn(s"Cache\tError\t${cacheKey}\tServing stale cache", e)
              stale.value.asInstanceOf[T]
            case None =>
              throw e
          }
      }
    }

    private def currentDecoded(cacheKey: String, version: Option[String]): Option[Decoded] =
      Option(decodedEntries.get(cacheKey))
        .filter(d => d.version.isDefined && d.version == version)
        .map { d => d.confirmedAtEpochMs = System.currentTimeMillis(); d }

    /**
     * The version is read before the value, never after. Read after, a write landing between the
     * two reads would pair the new version with the old value, and this process would keep
     * serving the old value for as long as the new version lasted. Read before, the worst case
     * pairs the old version with the new value, which the next read corrects.
     */
    private def fetch(cacheKey: String)(implicit i: I, encoder: JsonEncoder[T], decoder: JsonDecoder[T]): Decoded = {
      val version = syncCacheApi.get[String](cacheKey + VersionKeySuffix)
      currentDecoded(cacheKey, version).getOrElse {
        val decoded = syncCacheApi.get[String](cacheKey) match {
          case Some(json) =>
            new Decoded(version, decode(json))
          case None =>
            val json = wrapOrElse().toJson
            val newVersion = java.util.UUID.randomUUID().toString
            // Once, because some entities draw a random duration per call. The value is written
            // first so that whoever sees the new version can also see the value.
            val expire = durationExpire
            syncCacheApi.set(cacheKey, json, expire)
            syncCacheApi.set(cacheKey + VersionKeySuffix, newVersion, expire)
            new Decoded(Some(newVersion), decode(json))
        }
        rememberDecoded(cacheKey, decoded)
        decoded
      }
    }

    private def decode(json: String)(implicit i: I, decoder: JsonDecoder[T]): T =
      json.fromJson[T] match {
        case Left(e) =>
          logger.error(s"Cache\tParse\t${key()}\terror=$e")
          throw new RuntimeException(s"Cache\tParse\t${key()}\terror=$e")
        case Right(t) =>
          t
      }

    private def wrapOrElse()(implicit i: I): T = {
      StopWatch(Seq("Cache", "Miss", key()).mkString("\t")) {
        orElse()
      }
    }

    private def isStaleBackupExpired(entry: Decoded): Boolean = {
      val maxStaleMs = math.max(durationExpire.toMillis * 6, 60000L)
      System.currentTimeMillis() - entry.confirmedAtEpochMs > maxStaleMs
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
    Footer.invalidate()
    Config.invalidate()
  }

  object Footer extends CacheEntityWithContextSite[String] {
    override val durationExpire: FiniteDuration = if (environment.mode == Dev) 1 minute else 1 hour

    override def orElse()(implicit contextSite: ContextSite): String = contextSite.withConnection { implicit connection =>
      implicit val context: ContextWikiPage = contextSite.toContextWikiPage(Seq(""), RenderingMode.Normal)
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
    override def orElse()(implicit contextSite: ContextSite): String = contextSite.withConnection { implicit connection =>
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
      override def durationExpire: FiniteDuration = {
        if (environment.mode == Dev)
          FiniteDurationUtil.random(1.minutes, 2.minutes)
        else
          FiniteDurationUtil.random(5.minutes, 30.minutes)
      }
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
