package logics

import com.aha00a.commons.utils.StopWatch
import models.tables.Permission
import models.tables.Site
import play.api.Logging

import java.sql.Connection

object AhaWikiCacheMemoryPermission extends Logging {
  private val cache = new AhaWikiCacheMemoryTrieMap[Long, Seq[Permission]]

  def get()(implicit connection: Connection, site: Site): Seq[Permission] =
    cache.getOrElseUpdate(site.seq) {
      StopWatch("Cache\tMiss\tAhaWikiCacheMemoryPermission") {
        Permission.select()
      }
    }

  def invalidate(siteSeq: Long): Unit = cache.invalidate(siteSeq)
  def clear(): Unit = cache.clear()
}
