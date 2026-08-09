package com.aha00a.tests

import play.api.cache.SyncCacheApi

import scala.collection.concurrent.TrieMap
import scala.concurrent.duration.Duration
import scala.reflect.ClassTag

/**
 * The pieces every spec that boots a Play application needs.
 *
 * Four specs carried a byte-identical `TestSyncCacheApi` and near-identical Guice
 * configuration. What differed between them was worth keeping — `ApiV1FilterSpec` runs the
 * real filter chain on purpose — so the shared part is a starting map a spec adds to,
 * rather than a fixed configuration it has to accept whole.
 */
object TestApplication {

  /** In-memory stand-in: the real cache module is Redis, which tests do not run. */
  class TestSyncCacheApi extends SyncCacheApi {
    private val values = TrieMap.empty[String, Any]

    override def set(key: String, value: Any, expiration: Duration): Unit =
      values.put(key, value)

    override def remove(key: String): Unit =
      values.remove(key)

    override def getOrElseUpdate[A](key: String, expiration: Duration)(orElse: => A)(implicit evidence$1: ClassTag[A]): A =
      values.getOrElseUpdate(key, orElse).asInstanceOf[A]

    override def get[T](key: String)(implicit evidence$2: ClassTag[T]): Option[T] =
      values.get(key).map(_.asInstanceOf[T])
  }

  /**
   * A database name no other spec will collide with.
   *
   * Specs run in the same JVM and `DB_CLOSE_DELAY=-1` keeps an in-memory database alive for
   * its whole life, so two specs sharing a name would share tables.
   */
  def randomDbName(prefix: String): String =
    s"${prefix}_${java.util.UUID.randomUUID().toString.replace("-", "")}"

  /**
   * `NON_KEYWORDS=USER` matters because `USER` is reserved in H2 but is a table name here.
   * It is harmless for specs that never touch that table, so every spec gets it.
   */
  def h2Url(dbName: String): String =
    s"jdbc:h2:mem:$dbName;MODE=MySQL;NON_KEYWORDS=USER;DB_CLOSE_DELAY=-1"

  /**
   * Configuration shared by every spec: H2 instead of MySQL, no evolutions, and the modules
   * that reach outside the JVM disabled. Add to it or override a key to say how one spec
   * differs.
   */
  def baseConfiguration(dbName: String): Map[String, Any] = Map(
    "db.default.driver" -> "org.h2.Driver",
    "db.default.url" -> h2Url(dbName),
    "db.default.username" -> "sa",
    "db.default.password" -> "",
    "play.evolutions.db.default.enabled" -> false,
    "play.modules.disabled" -> Seq(
      "play.api.cache.redis.RedisCacheModule",
      "services.ApplicationLifecycleHook",
    ),
    "play.http.filters" -> "play.api.http.NoHttpFilters",
    "play.http.secret.key" -> "test-secret-key-for-testing-only",
    "AhaWiki.accessLog.sampleRate" -> 0,
  )
}
