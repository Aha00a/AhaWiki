package logics

import models.tables.CalculatedLink

import java.time.Instant
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
  private val cache = new AhaWikiCacheMemoryTrieMap[(Long, String), Seq[CalculatedLink]]

  def getOrElseUpdate(siteSeq: Long, pageName: String)(orElse: => Seq[CalculatedLink]): Seq[CalculatedLink] =
    cache.getOrElseUpdate((siteSeq, pageName))(orElse)

  def clear(): Unit = cache.clear()

  def invalidate(siteSeq: Long, pageName: String): Unit = cache.invalidate((siteSeq, pageName))

  def snapshot(instancePort: String): AhaWikiCacheMemoryApiLinks.Snapshot =
    AhaWikiCacheMemoryApiLinks.Snapshot(
      instancePort = instancePort,
      capturedAtEpochMs = System.currentTimeMillis(),
      capturedAtIso8601 = Instant.now().toString,
      linksCacheKeyCount = cache.size,
      linksCacheValueCount = cache.values.map(_.size.toLong).sum,
    )
}
