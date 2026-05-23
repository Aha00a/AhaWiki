package logics

import com.aha00a.commons.utils.StopWatch
import models.tables.SiteAdmin
import play.api.Logging

import java.sql.Connection

object AhaWikiCacheMemorySiteAdmin extends Logging {
  private val cache = new AhaWikiCacheMemoryTrieMap[Long, Set[Long]]

  def getUserSeqs(siteSeq: Long)(implicit connection: Connection): Set[Long] =
    cache.getOrElseUpdate(siteSeq) {
      StopWatch("Cache\tMiss\tAhaWikiCacheMemorySiteAdmin") {
        SiteAdmin.selectBySite(siteSeq).map(_.user).toSet
      }
    }

  def invalidate(siteSeq: Long): Unit = cache.invalidate(siteSeq)
  def clear(): Unit = cache.clear()
}
