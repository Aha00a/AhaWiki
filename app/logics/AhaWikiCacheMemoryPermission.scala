package logics

import com.aha00a.commons.utils.StopWatch
import models.tables.Permission
import models.tables.Site
import org.joda.time.DurationFieldType.hours
import org.joda.time.PeriodType.hours
import play.api.Logging

import java.sql.Connection
import scala.collection.concurrent.TrieMap
import scala.concurrent.duration.DurationInt

object AhaWikiCacheMemoryPermission extends Logging {
  private case class CachedPermissions(value: Seq[Permission], cachedAtEpochMs: Long)

  private val permissionsBySiteSeq = TrieMap.empty[Long, CachedPermissions]
  private val permissionsCacheTtlMs: Long = (6 hours).toMillis

  private def isExpired(entry: CachedPermissions, now: Long): Boolean = now - entry.cachedAtEpochMs > permissionsCacheTtlMs

  def get()(implicit connection: Connection, site: Site): Seq[Permission] = {
    val now = System.currentTimeMillis()
    permissionsBySiteSeq.get(site.seq).filterNot(entry => isExpired(entry, now)).map(_.value).getOrElse {
      StopWatch("Cache\tMiss\tAhaWikiCacheMemoryPermission") {
        val permissions = Permission.select()
        permissionsBySiteSeq.put(site.seq, CachedPermissions(permissions, now))
        permissions
      }
    }
  }

  def invalidate(siteSeq: Long): Unit = {
    permissionsBySiteSeq.remove(siteSeq)
  }

  def clear(): Unit = {
    permissionsBySiteSeq.clear()
  }
}
