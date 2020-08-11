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

object PageLogic {

  import models.WikiContext.Provider
  import models.tables.PageWithoutContentWithSize

  def insert(name: String, revision: Long, dateTime: Date, comment: String, body: String)(implicit wikiContext: WikiContext): Unit = {
    wikiContext.database.withConnection { implicit connection =>
      import models.tables.Page
      implicit val syncCacheApi: SyncCacheApi = wikiContext.syncCacheApi
      val author = wikiContext.provider.getId.getOrElse("anonymous")
      val permRead = PageContent(body).read.getOrElse("")
      val page = Page(name, revision, dateTime, author, wikiContext.provider.remoteAddress, comment, permRead, body)
      Page.insert(page)
      wikiContext.actorAhaWiki ! Calculate(name)
      AhaWikiCache.PageList.invalidate()
      name match {
        case ".header" => AhaWikiCache.Header.invalidate()
        case ".footer" => AhaWikiCache.Footer.invalidate()
        case ".config" => AhaWikiCache.Config.invalidate()
        case _ =>
      }
    }
  }

  def getListPage()(implicit syncCacheApi: SyncCacheApi, database:Database): List[PageWithoutContentWithSize] = {
    AhaWikiCache.PageList.get()
  }

  def getListPageByPermission()(implicit provider: Provider, syncCacheApi: SyncCacheApi, database:Database): List[PageWithoutContentWithSize] = {
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
