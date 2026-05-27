package logics

import java.util.concurrent.ConcurrentHashMap
import scala.jdk.CollectionConverters._

class AhaWikiCacheMemoryTrieMap[K, V](maxSize: Int = Int.MaxValue) {
  private val cache = new ConcurrentHashMap[K, V]()

  def getOrElseUpdate(key: K)(orElse: => V): V = {
    if (cache.size() >= maxSize) evictHalf()
    cache.computeIfAbsent(key, _ => orElse)
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
  def values: Iterable[V] = cache.values().asScala
}
