package logics

import models.tables.CalculatedLink

import java.lang.management.ManagementFactory
import java.time.Instant
import scala.collection.concurrent.TrieMap

object ApiLinksMemoryCache {
  case class Snapshot(
    instancePort: String,
    capturedAtEpochMs: Long,
    capturedAtIso8601: String,
    linksCacheKeyCount: Int,
    linksCacheValueCount: Long,
    jvmUsedMemoryBytes: Long,
    jvmTotalMemoryBytes: Long,
    jvmMaxMemoryBytes: Long,
    jvmThreadCount: Int,
  )
}

class ApiLinksMemoryCache {
  private case class CachedLinks(value: Seq[CalculatedLink], cachedAtEpochMs: Long)
  private val linksCache = TrieMap.empty[(Long, String), CachedLinks]
  private val linksCacheTtlMs: Long = 10 * 60 * 1000
  private val cleanupIntervalMs: Long = 60 * 1000
  @volatile private var lastCleanupEpochMs: Long = 0L

  private def isExpired(entry: CachedLinks, now: Long): Boolean = now - entry.cachedAtEpochMs > linksCacheTtlMs

  private def cleanupExpired(now: Long): Unit = {
    if (now - lastCleanupEpochMs >= cleanupIntervalMs) {
      linksCache.foreach { case (key, entry) =>
        if (isExpired(entry, now)) {
          linksCache.remove(key, entry)
        }
      }
      lastCleanupEpochMs = now
    }
  }

  def getOrElseUpdate(siteSeq: Long, pageName: String)(orElse: => Seq[CalculatedLink]): Seq[CalculatedLink] = {
    val cacheKey = (siteSeq, pageName)
    val now = System.currentTimeMillis()
    cleanupExpired(now)
    val cached = linksCache.get(cacheKey).filterNot(entry => isExpired(entry, now))
    cached.map(_.value).getOrElse {
      val fetched = orElse
      linksCache.put(cacheKey, CachedLinks(fetched, now))
      fetched
    }
  }

  def clear(): Unit = linksCache.clear()

  def snapshot(instancePort: String): ApiLinksMemoryCache.Snapshot = {
    val runtime = Runtime.getRuntime
    ApiLinksMemoryCache.Snapshot(
      instancePort = instancePort,
      capturedAtEpochMs = System.currentTimeMillis(),
      capturedAtIso8601 = Instant.now().toString,
      linksCacheKeyCount = linksCache.size,
      linksCacheValueCount = linksCache.values.map(_.value.size.toLong).sum,
      jvmUsedMemoryBytes = runtime.totalMemory() - runtime.freeMemory(),
      jvmTotalMemoryBytes = runtime.totalMemory(),
      jvmMaxMemoryBytes = runtime.maxMemory(),
      jvmThreadCount = ManagementFactory.getThreadMXBean.getThreadCount,
    )
  }
}
