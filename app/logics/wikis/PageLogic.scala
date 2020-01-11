package logics.wikis

import java.util.Date

import actors.ActorAhaWiki.Calculate
import com.aha00a.commons.Implicits._
import com.aha00a.play.Implicits._
import logics.{AhaWikiCache, AhaWikiConfig, SessionLogic}
import models.{AhaWikiQuery, Page, PageContent, PageWithoutContentWithSize, WikiContext}
import play.api.cache.CacheApi
import play.api.db.Database
import play.api.mvc.Request

object PageLogic {
  def insert(name: String, revision: Long, dateTime: Date, comment: String, body: String)(implicit wikiContext: WikiContext): Unit = {
    wikiContext.database.withConnection { implicit connection =>
      val request = wikiContext.request
      val author = SessionLogic.getId(request).getOrElse("anonymous")
      val remoteAddress = request.remoteAddressWithXRealIp
      val permRead = PageContent(body).read.getOrElse("")
      val page = Page(name, revision, dateTime, author, remoteAddress, comment, permRead, body)
      val ahaWikiQuery = AhaWikiQuery()
      ahaWikiQuery.Page.insert(page)
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

  def getListPageWithoutContentWithSize()(implicit request: Request[Any], cacheApi: CacheApi, database:Database): List[PageWithoutContentWithSize] = {
    val permissionDefaultRead = AhaWikiConfig().permission.default.read()
    val permissionDefaultReadSplit = permissionDefaultRead.splitCommaIgnoreAroundWhitespace()
    val wikiPermission = WikiPermission()
    val optionId = SessionLogic.getId(request)
    val list: List[PageWithoutContentWithSize] = AhaWikiCache.PageList.get()
    val listFiltered = list.filter(p => {
      wikiPermission.allowed(optionId, p.permRead.toOption.map(_.splitCommaIgnoreAroundWhitespace()).getOrElse(permissionDefaultReadSplit))
    })
    listFiltered
  }
}
