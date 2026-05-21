package services

import com.aha00a.commons.utils.StopWatch
import logics.AhaWikiCache
import logics.AhaWikiCacheMemoryApiLinks
import logics.ApplicationConf
import logics.wikis.PageLogic
import models.RequestWrapper
import models.WikiActors
import models.tables.Site
import play.api.Logger
import play.api.Logging
import play.api.db.Database

import javax.inject.Inject

class ServicePageCalculator @Inject()(
  database: Database,
  applicationConf: ApplicationConf,
  ahaWikiCache: AhaWikiCache,
  ahaWikiCacheMemoryApiLinks: AhaWikiCacheMemoryApiLinks,
) extends Logging {
  def calculate(site: Site, name: String, i: Int = 0, length: Int = 1)(implicit wikiActors: WikiActors): Unit = {
    StopWatch(s"Calculate\t${site.name}(${site.seq})\t$name\t(${i + 1}/$length)") {
      database.withConnection { implicit connection =>
        implicit val implicitDatabase: Database = database
        implicit val implicitApplicationConf: ApplicationConf = applicationConf
        implicit val implicitAhaWikiCache: AhaWikiCache = ahaWikiCache
        implicit val implicitLogger: Logger = logger
        implicit val implicitSite: Site = site
        implicit val provider: RequestWrapper = RequestWrapper.empty

        PageLogic.calculate(name)
        ahaWikiCacheMemoryApiLinks.invalidate(site.seq, name)
      }
    }
  }
}
