package logics.wikis

import java.util.Date
import actors.ActorAhaWiki.Calculate
import com.aha00a.commons.Implicits._
import logics.AhaWikiConfig
import models._
import models.tables.Page
import play.api.db.Database

import java.sql.Connection

object PageLogic {

  import models.ContextSite.RequestWrapper
  import models.tables.PageWithoutContentWithSize
  import models.tables.Site

  def insert(name: String, revision: Long, dateTime: Date, comment: String, body: String)(implicit wikiContext: ContextWikiPage, connection: Connection): Unit = {
    import models.tables.Page
    import models.tables.Site
    implicit val site: Site = wikiContext.site
    val author = wikiContext.requestWrapper.getId.getOrElse("anonymous")
    val permRead = PageContent(body).read.getOrElse("")
    val page = Page(name, revision, dateTime, author, wikiContext.requestWrapper.remoteAddress, comment, permRead, body)
    Page.insert(page)
    wikiContext.actorAhaWiki ! Calculate(site, name)
  }

  def getListPage()(implicit connection: Connection, site: Site): List[PageWithoutContentWithSize] = {
    Page.pageSelectPageList()
  }

  def getListPageByPermission()(implicit provider: RequestWrapper, database: Database, connection: Connection, site: Site): List[PageWithoutContentWithSize] = {
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
