package logics

import java.util.concurrent.ConcurrentHashMap
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._

object AhaWikiCacheMemoryTrieMap {
  /**
   * How long an instance may go on applying access rows it loaded before.
   *
   * Production runs two instances behind one proxy and both answer requests (measured
   * 2026-09-10: Dev Deploying). These caches live in one JVM each, and a change invalidates them
   * only on the instance that handled it. Until this bound existed the other instance kept its
   * copy until a six-hourly clear, so a page just made private stayed readable through half the
   * traffic, and a removed site admin kept the admin screens there. Reloading costs one small
   * indexed query per site, so the bound can be short.
   */
  val AccessDecisionMaxAge: FiniteDuration = 10.seconds
}

/** A bounded in-process map. With `maxAge`, an entry older than that is loaded again on its next read. */
class AhaWikiCacheMemoryTrieMap[K, V](
  maxSize: Int = Int.MaxValue,
  maxAge: Option[FiniteDuration] = None,
  nanoTime: () => Long = () => System.nanoTime(),
) {
  private case class Entry(value: V, loadedAt: Long)

  private val cache = new ConcurrentHashMap[K, Entry]()
  private val maxAgeNanos: Long = maxAge.map(_.toNanos).getOrElse(Long.MaxValue)

  private def fresh(entry: Entry): Boolean = nanoTime() - entry.loadedAt < maxAgeNanos

  def getOrElseUpdate(key: K)(orElse: => V): V = {
    val existing = cache.get(key)
    if (existing != null && fresh(existing)) {
      existing.value
    } else {
      if (cache.size() >= maxSize) evictHalf()
      // compute, not computeIfAbsent: a stale entry is present and still has to be replaced.
      // Two readers racing on one expired key load it once, as computeIfAbsent did for a missing one.
      cache.compute(key, (_: K, current: Entry) =>
        if (current != null && fresh(current)) current else Entry(orElse, nanoTime())
      ).value
    }
  }

  private def evictHalf(): Unit = {
    val target = maxSize / 2
    val iter   = cache.keySet().iterator()
    while (iter.hasNext && cache.size() > target) {
      iter.next()
      iter.remove()
    }
  }
  def invalidate(key: K): Unit = cache.remove(key)
  def clear(): Unit = cache.clear()
  def size: Int = cache.size
  def values: Iterable[V] = cache.values().asScala.map(_.value)
}
