package logics

import com.aha00a.commons.Implicits.RichSeq
import models.tables.Site
import play.api.db.Database

import java.sql.Connection

/** The connection parameters are AhaWikiCacheMemoryDomainSite's: why they default to null is there. */
object SiteLogic {
  def get(host: String)(implicit database: Database, connection: Connection = null): Site = {
    AhaWikiCacheMemoryDomainSite
      .get(host)
      .getOrElse(Site.notFound)
  }

  def getBySeq(seq: Long)(implicit database: Database, connection: Connection = null): Option[Site] = AhaWikiCacheMemoryDomainSite.getSite(seq)
  def selectRandom()(implicit database: Database, connection: Connection = null): Site = AhaWikiCacheMemoryDomainSite.getSites().random()
}
