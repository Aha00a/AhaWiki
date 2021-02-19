package logics.wikis

import java.util.Date

import actors.ActorAhaWiki.Calculate
import com.aha00a.commons.Implicits._
import com.aha00a.play.Implicits._
import logics.{AhaWikiCache, AhaWikiConfig, SessionLogic}
import models._
import play.api.cache.SyncCacheApi
import play.api.db.Database
import play.api.mvc.Request
import models.tables.Page

object PageLogic {

  import models.ContextSite.Provider
  import models.tables.PageWithoutContentWithSize
  import models.tables.Site

  // TODO: add connection
  def insert(name: String, revision: Long, dateTime: Date, comment: String, body: String)(implicit wikiContext: ContextWikiPage): Unit = {
    wikiContext.database.withConnection { implicit connection =>
      import models.tables.Page
      import models.tables.Site
      implicit val site: Site = wikiContext.site
      val author = wikiContext.provider.getId.getOrElse("anonymous")
      val permRead = PageContent(body).read.getOrElse("")
      val page = Page(name, revision, dateTime, author, wikiContext.provider.remoteAddress, comment, permRead, body)
      Page.insert(page)
      wikiContext.actorAhaWiki ! Calculate(site, name)
    }
  }

  // TODO: add connection
  def getListPage()(implicit database:Database, site: Site): List[PageWithoutContentWithSize] = {
    database.withConnection { implicit connection =>
      Page.pageSelectPageList()
    }
  }

  def getListPageByPermission()(implicit provider: Provider, database:Database, site: Site): List[PageWithoutContentWithSize] = {
    val permissionDefaultRead = AhaWikiConfig().permission.default.read()
    val permissionDefaultReadSplit = permissionDefaultRead.splitCommaIgnoreAroundWhitespace()
    val wikiPermission = WikiPermission()
    val optionId = provider.getId
    val list: List[PageWithoutContentWithSize] = getListPage()
    val listFiltered = list.filter(p => {
      wikiPermission.allowed(optionId, p.permRead.toOption.map(_.splitCommaIgnoreAroundWhitespace()).getOrElse(permissionDefaultReadSplit))
    })
    listFiltered
  }
  
}
