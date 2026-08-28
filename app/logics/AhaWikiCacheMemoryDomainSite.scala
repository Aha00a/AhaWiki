package logics

import com.aha00a.commons.utils.StopWatch
import models.tables.Site
import models.tables.SiteDomain
import play.api.Logging
import play.api.db.Database


object AhaWikiCacheMemoryDomainSite extends Logging {
  @volatile private var domainToSite: Map[String, Site] = Map.empty
  @volatile private var siteBySeq: Map[Long, Site] = Map.empty

  def get(host: String)(implicit database: Database): Option[Site] = {
    val snapshot = domainToSite
    if (snapshot.nonEmpty) snapshot.get(host)
    else refreshIfNeeded()._1.get(host)
  }

  def getSite(seq: Long)(implicit database: Database): Option[Site] = {
    val snapshot = siteBySeq
    if (snapshot.nonEmpty) snapshot.get(seq)
    else refreshIfNeeded()._2.get(seq)
  }

  def getSites()(implicit database: Database): Seq[Site] = {
    val snapshot = siteBySeq
    if (snapshot.nonEmpty) snapshot.values.toSeq
    else refreshIfNeeded()._2.values.toSeq
  }

  def invalidate(): Unit = synchronized {
    domainToSite = Map.empty
    siteBySeq = Map.empty
  }

  def refresh()(implicit database: Database): Unit = synchronized {
    StopWatch("Cache\tMiss\tAhaWikiCacheDomainSite") {
      val (loadedSiteBySeq, mapped) = database.withConnection { implicit connection =>
        val loadedSiteBySeq = Site.select().map(site => (site.seq, site)).toMap
        val mapped = SiteDomain.select().flatMap(sd => loadedSiteBySeq.get(sd.site).map(site => (sd.domain, site))).toMap
        (loadedSiteBySeq, mapped)
      }
      siteBySeq = loadedSiteBySeq
      domainToSite = mapped
    }
  }

  private def refreshIfNeeded()(implicit database: Database): (Map[String, Site], Map[Long, Site]) = synchronized {
    if (domainToSite.nonEmpty && siteBySeq.nonEmpty) {
      (domainToSite, siteBySeq)
    } else {
      refresh()
      (domainToSite, siteBySeq)
    }
  }
}
