package services

import com.aha00a.commons.utils.StopWatch
import logics.AhaWikiCache
import logics.AhaWikiCacheMemoryApiLinks
import logics.ApplicationConf
import logics.wikis.PageLogic
import models.RequestWrapper
import models.WikiActors
import models.tables.Page
import models.tables.PageMeta
import models.tables.Site
import play.api.Logger
import play.api.Logging
import play.api.db.Database

import java.time.LocalDateTime
import javax.inject.Inject

class ServicePageCalculator @Inject()(
  database: Database,
  applicationConf: ApplicationConf,
  ahaWikiCache: AhaWikiCache,
  ahaWikiCacheMemoryApiLinks: AhaWikiCacheMemoryApiLinks,
) extends Logging {
  def calculate(site: Site, name: String, i: Int = 0, length: Int = 1, onlyIfStale: Boolean = false)(implicit wikiActors: WikiActors): Unit = {
    database.withConnection { implicit connection =>
      implicit val implicitSite: Site = site
      // Checked before the stopwatch, so a skipped view leaves no "Calculate" line: the count of
      // those lines is how the calculations actually run are measured.
      val skip = onlyIfStale && PageLogic.isCalculatedRecently(PageMeta.select(name), Page.selectLastRevisionNumber(name), LocalDateTime.now())
      if (!skip) {
        StopWatch(s"Calculate\t${site.name}(${site.seq})\t$name\t(${i + 1}/$length)") {
          implicit val implicitDatabase: Database = database
          implicit val implicitApplicationConf: ApplicationConf = applicationConf
          implicit val implicitAhaWikiCache: AhaWikiCache = ahaWikiCache
          implicit val implicitLogger: Logger = logger
          implicit val provider: RequestWrapper = RequestWrapper.empty

          PageLogic.calculate(name)
          ahaWikiCacheMemoryApiLinks.invalidate(site.seq, name)
        }
      }
    }
  }
}
