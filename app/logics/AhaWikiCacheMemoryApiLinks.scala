package logics

import models.tables.CalculatedLink

import java.time.Instant
import scala.collection.concurrent.TrieMap
import javax.inject.Singleton

object AhaWikiCacheMemoryApiLinks {
  case class Snapshot(
    instancePort: String,
    capturedAtEpochMs: Long,
    capturedAtIso8601: String,
    linksCacheKeyCount: Int,
    linksCacheValueCount: Long,
  )
}

@Singleton
class AhaWikiCacheMemoryApiLinks {
  private case class CachedLinks(value: Seq[CalculatedLink], cachedAtEpochMs: Long)
  private val linksCache = TrieMap.empty[(Long, String), CachedLinks]
  private val linksCacheTtlMs: Long = 10 * 60 * 1000

  private def isExpired(entry: CachedLinks, now: Long): Boolean = now - entry.cachedAtEpochMs > linksCacheTtlMs

  private def cleanupExpired(now: Long): Unit = {
    linksCache.foreach { case (key, entry) =>
      if (isExpired(entry, now)) {
        linksCache.remove(key, entry)
      }
    }
  }

  def cleanupExpiredNow(): Unit = cleanupExpired(System.currentTimeMillis())

  def getOrElseUpdate(siteSeq: Long, pageName: String)(orElse: => Seq[CalculatedLink]): Seq[CalculatedLink] = {
    val cacheKey = (siteSeq, pageName)
    val now = System.currentTimeMillis()
    val cached = linksCache.get(cacheKey).filterNot(entry => isExpired(entry, now))
    cached.map(_.value).getOrElse {
      val fetched = orElse
      linksCache.put(cacheKey, CachedLinks(fetched, now))
      fetched
    }
  }

  def clear(): Unit = linksCache.clear()

  def invalidate(siteSeq: Long, pageName: String): Unit = {
    linksCache.remove((siteSeq, pageName))
  }

  def snapshot(instancePort: String): AhaWikiCacheMemoryApiLinks.Snapshot = {
    AhaWikiCacheMemoryApiLinks.Snapshot(
      instancePort = instancePort,
      capturedAtEpochMs = System.currentTimeMillis(),
      capturedAtIso8601 = Instant.now().toString,
      linksCacheKeyCount = linksCache.size,
      linksCacheValueCount = linksCache.values.map(_.value.size.toLong).sum,
    )
  }
}
