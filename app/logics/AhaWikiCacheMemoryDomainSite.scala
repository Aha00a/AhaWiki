package logics

import com.aha00a.commons.utils.StopWatch
import models.tables.Site
import models.tables.SiteDomain
import play.api.Logging
import play.api.db.Database

import java.sql.Connection


/**
 * Every site and the domains it answers to, in memory.
 *
 * Almost every request asks this for its site first thing, and most ask from inside the
 * connection they have just opened. When the maps are empty -- the first seconds after start,
 * before the scheduled refresh, and after [[invalidate]] -- the ask loads them, and until
 * 2026-11-18 it loaded them over a second connection from the pool, inside `synchronized`. With
 * N connections in the pool and N requests at that moment, each held one and waited for
 * another; the rest waited on the lock. Running every spec with a pool of one found it in 22
 * places.
 *
 * So each method takes the caller's connection when there is one in implicit scope, and loads
 * over that. The default `null` is how a caller with no connection says so -- an implicit
 * parameter cannot default to `None` and still be filled by an implicit `Connection` -- and
 * those load over a connection of their own, holding nothing else.
 */
object AhaWikiCacheMemoryDomainSite extends Logging {
  @volatile private var domainToSite: Map[String, Site] = Map.empty
  @volatile private var siteBySeq: Map[Long, Site] = Map.empty

  def get(host: String)(implicit database: Database, connection: Connection = null): Option[Site] = {
    val snapshot = domainToSite
    if (snapshot.nonEmpty) snapshot.get(host)
    else refreshIfNeeded()._1.get(host)
  }

  def getSite(seq: Long)(implicit database: Database, connection: Connection = null): Option[Site] = {
    val snapshot = siteBySeq
    if (snapshot.nonEmpty) snapshot.get(seq)
    else refreshIfNeeded()._2.get(seq)
  }

  def getSites()(implicit database: Database, connection: Connection = null): Seq[Site] = {
    val snapshot = siteBySeq
    if (snapshot.nonEmpty) snapshot.values.toSeq
    else refreshIfNeeded()._2.values.toSeq
  }

  def invalidate(): Unit = synchronized {
    domainToSite = Map.empty
    siteBySeq = Map.empty
  }

  def refresh()(implicit database: Database, connection: Connection = null): Unit = synchronized {
    StopWatch("Cache\tMiss\tAhaWikiCacheDomainSite") {
      def load(c: Connection): (Map[Long, Site], Map[String, Site]) = {
        val loadedSiteBySeq = Site.select()(c).map(site => (site.seq, site)).toMap
        val mapped = SiteDomain.select()(c).flatMap(sd => loadedSiteBySeq.get(sd.site).map(site => (sd.domain, site))).toMap
        (loadedSiteBySeq, mapped)
      }
      val (loadedSiteBySeq, mapped) = Option(connection).filterNot(_.isClosed) match {
        case Some(held) => load(held)
        case None => database.withConnection(c => load(c))
      }
      siteBySeq = loadedSiteBySeq
      domainToSite = mapped
    }
  }

  private def refreshIfNeeded()(implicit database: Database, connection: Connection): (Map[String, Site], Map[Long, Site]) = synchronized {
    if (domainToSite.nonEmpty && siteBySeq.nonEmpty) {
      (domainToSite, siteBySeq)
    } else {
      refresh()
      (domainToSite, siteBySeq)
    }
  }
}
