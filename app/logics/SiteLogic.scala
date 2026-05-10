package logics

import com.aha00a.commons.Implicits.RichSeq
import models.tables.Site
import play.api.db.Database

object SiteLogic {
  def get(host: String)(implicit database: Database): Site = {
    AhaWikiCacheMemoryDomainSite
      .get(host)
      .getOrElse(Site.notFound)
  }

  def get(seq: Long)(implicit database: Database): Option[Site] = AhaWikiCacheMemoryDomainSite.getSite(seq)
  def selectRandom()(implicit database: Database): Site = AhaWikiCacheMemoryDomainSite.getSites().random()
}
