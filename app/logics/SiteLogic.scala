package logics

import com.aha00a.commons.Implicits.RichSeq
import models.tables.Site
import play.api.db.Database

object SiteLogic {
  def get(host: String)(implicit database: Database, ahaWikiCache: AhaWikiCache): Site = {
    ahaWikiCache.SiteDomain.Map
      .get()
      .get(host)
      .flatMap(sd => {ahaWikiCache.Site.Map.get().get(sd.site)})
      .getOrElse(Site.notFound)
  }

  def selectRandom()(implicit database: Database, ahaWikiCache: AhaWikiCache): Site = ahaWikiCache.Site.get().random()
}
