package logics

import com.aha00a.commons.utils.StopWatch
import models.tables.SiteAdmin
import play.api.Logging

import java.sql.Connection
import scala.collection.concurrent.TrieMap
import scala.concurrent.duration.DurationInt

object AhaWikiCacheMemorySiteAdmin extends Logging {
  private case class CachedSiteAdmins(userSeqs: Set[Long], cachedAtEpochMs: Long)

  private val siteAdminsBySiteSeq = TrieMap.empty[Long, CachedSiteAdmins]
  private val cacheTtlMs: Long = (6 hours).toMillis

  private def isExpired(entry: CachedSiteAdmins, now: Long): Boolean =
    now - entry.cachedAtEpochMs > cacheTtlMs

  def getUserSeqs(siteSeq: Long)(implicit connection: Connection): Set[Long] = {
    val now = System.currentTimeMillis()
    siteAdminsBySiteSeq.get(siteSeq).filterNot(entry => isExpired(entry, now)).map(_.userSeqs).getOrElse {
      StopWatch("Cache\tMiss\tAhaWikiCacheMemorySiteAdmin") {
        val userSeqs = SiteAdmin.selectBySite(siteSeq).map(_.user).toSet
        siteAdminsBySiteSeq.put(siteSeq, CachedSiteAdmins(userSeqs, now))
        userSeqs
      }
    }
  }

  def invalidate(siteSeq: Long): Unit = {
    siteAdminsBySiteSeq.remove(siteSeq)
  }

  def clear(): Unit = {
    siteAdminsBySiteSeq.clear()
  }
}
