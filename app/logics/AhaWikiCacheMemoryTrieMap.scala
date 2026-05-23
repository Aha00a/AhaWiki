package logics

import scala.collection.concurrent.TrieMap

class AhaWikiCacheMemoryTrieMap[K, V] {
  private val cache = TrieMap.empty[K, V]

  def getOrElseUpdate(key: K)(orElse: => V): V = cache.getOrElseUpdate(key, orElse)
  def invalidate(key: K): Unit = cache.remove(key)
  def clear(): Unit = cache.clear()
  def size: Int = cache.size
  def values: Iterable[V] = cache.values
}
