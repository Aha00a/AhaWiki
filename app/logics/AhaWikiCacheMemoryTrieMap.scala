package logics

import java.util.concurrent.ConcurrentHashMap
import scala.jdk.CollectionConverters._

class AhaWikiCacheMemoryTrieMap[K, V] {
  private val cache = new ConcurrentHashMap[K, V]()

  def getOrElseUpdate(key: K)(orElse: => V): V =
    cache.computeIfAbsent(key, _ => orElse)
  def invalidate(key: K): Unit = cache.remove(key)
  def clear(): Unit = cache.clear()
  def size: Int = cache.size
  def values: Iterable[V] = cache.values().asScala
}
