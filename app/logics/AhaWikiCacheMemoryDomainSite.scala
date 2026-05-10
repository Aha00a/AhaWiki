package logics

import models.tables.Site
import models.tables.SiteDomain
import play.api.Logging
import play.api.db.Database
import scala.annotation.unused

object AhaWikiCacheMemoryDomainSite extends Logging {
  @volatile private var domainToSite: Map[String, Site] = Map.empty
  @volatile private var siteBySeq: Map[Long, Site] = Map.empty
  @volatile private var loadedAtEpochMs: Long = 0L
  private val durationMs: Long = 5 * 60 * 1000L

  def get(host: String)(implicit database: Database): Option[Site] = {
    val now = System.currentTimeMillis()
    val snapshot = domainToSite
    if (snapshot.nonEmpty && now - loadedAtEpochMs <= durationMs) {
      snapshot.get(host)
    } else {
      refreshIfNeeded()._1.get(host)
    }
  }

  def getSite(seq: Long)(implicit database: Database): Option[Site] = {
    val now = System.currentTimeMillis()
    val snapshot = siteBySeq
    if (snapshot.nonEmpty && now - loadedAtEpochMs <= durationMs) snapshot.get(seq)
    else refreshIfNeeded()._2.get(seq)
  }

  def getSites()(implicit database: Database): Seq[Site] = {
    val now = System.currentTimeMillis()
    val snapshot = siteBySeq
    if (snapshot.nonEmpty && now - loadedAtEpochMs <= durationMs) snapshot.values.toSeq
    else refreshIfNeeded()._2.values.toSeq
  }

  def invalidate()(implicit @unused database: Database): Unit = synchronized {
    domainToSite = Map.empty
    siteBySeq = Map.empty
    loadedAtEpochMs = 0L
  }

  private def refreshIfNeeded()(implicit database: Database): (Map[String, Site], Map[Long, Site]) = synchronized {
    val now = System.currentTimeMillis()
    if (domainToSite.nonEmpty && siteBySeq.nonEmpty && now - loadedAtEpochMs <= durationMs) {
      (domainToSite, siteBySeq)
    } else {
      val (loadedSiteBySeq, mapped) = database.withConnection { implicit connection =>
        val loadedSiteBySeq = Site.select().map(site => (site.seq, site)).toMap
        val mapped = SiteDomain.select().flatMap(sd => loadedSiteBySeq.get(sd.site).map(site => (sd.domain, site))).toMap
        (loadedSiteBySeq, mapped)
      }
      siteBySeq = loadedSiteBySeq
      domainToSite = mapped
      loadedAtEpochMs = now
      logger.info(s"Cache\tFill\tAhaWikiCacheDomainSite.sites=${loadedSiteBySeq.size}\tdomains=${mapped.size}")
      (mapped, loadedSiteBySeq)
    }
  }
}
