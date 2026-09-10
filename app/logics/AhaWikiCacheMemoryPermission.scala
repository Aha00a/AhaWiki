package logics

import com.aha00a.commons.utils.StopWatch
import models.tables.Permission
import models.tables.Site
import play.api.Logging

import java.sql.Connection

object AhaWikiCacheMemoryPermission extends Logging {
  // invalidate() reaches only the instance that handled the change; the age limit is what
  // reaches the other one. See AhaWikiCacheMemoryTrieMap.AccessDecisionMaxAge.
  private val cache = new AhaWikiCacheMemoryTrieMap[Long, Seq[Permission]](maxAge = Some(AhaWikiCacheMemoryTrieMap.AccessDecisionMaxAge))

  def get()(implicit connection: Connection, site: Site): Seq[Permission] =
    cache.getOrElseUpdate(site.seq) {
      StopWatch(s"Cache\tMiss\tAhaWikiCacheMemoryPermission\t${site}") {
        Permission.select()
      }
    }

  def invalidate(siteSeq: Long): Unit = cache.invalidate(siteSeq)
  def clear(): Unit = cache.clear()
}
