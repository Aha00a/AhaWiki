package logics.wikis

import java.util.Date

import actors.ActorAhaWiki.Calculate
import com.aha00a.play.Implicits._
import logics.{AhaWikiCache, SessionLogic}
import models.{AhaWikiQuery, Page, PageContent, WikiContext}

object PageLogic {
  def insert(name: String, revision: Long, dateTime: Date, body: String, comment: String)(implicit wikiContext: WikiContext): Unit = {
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
}
