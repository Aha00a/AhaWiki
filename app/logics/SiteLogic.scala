package logics

import com.aha00a.commons.Implicits.RichSeq
import models.tables.Site
import play.api.db.Database

object SiteLogic {
  def get(host: String)(implicit database: Database, ahaWikiCache: AhaWikiCache): Site = {
    AhaWikiCacheDomainSite
      .get(host)
      .getOrElse(Site.notFound)
  }

  def get(seq: Long)(implicit database: Database, ahaWikiCache: AhaWikiCache): Option[Site] = AhaWikiCacheDomainSite.getSite(seq)
  def selectRandom()(implicit database: Database, ahaWikiCache: AhaWikiCache): Site = AhaWikiCacheDomainSite.getSites().random()
}
