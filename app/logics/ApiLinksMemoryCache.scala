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

  def getOrElseUpdate(siteSeq: Long, pageName: String)(orElse: => Seq[CalculatedLink]): Seq[CalculatedLink] = {
    val cacheKey = (siteSeq, pageName)
    val now = System.currentTimeMillis()
    val cached = linksCache.get(cacheKey).filter(entry => now - entry.cachedAtEpochMs <= linksCacheTtlMs)
    cached.map(_.value).getOrElse {
      val fetched = orElse
      linksCache.put(cacheKey, CachedLinks(fetched, now))
      fetched
    }
  }

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
