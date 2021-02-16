package logics.wikis

import logics.{AhaWikiConfig, SessionLogic}
import models.WikiContext.Provider
import models.tables.Site
import models.{PageContent, WikiContext}
import play.api.cache.SyncCacheApi
import play.api.db.Database
import play.api.mvc.Request

object WikiPermission {
  def apply()(implicit provider: Provider, syncCacheApi: SyncCacheApi, database:Database, site: Site): WikiPermission = new WikiPermission()
}

class WikiPermission(implicit provider: Provider, syncCacheApi: SyncCacheApi, database:Database, site: Site) {
  def getReadDirective(pageContent:Option[PageContent]): Array[String] = {
    pageContent.flatMap(_.read).getOrElse(AhaWikiConfig().permission.default.read()).split("""\s*,\s*""")
  }

  def getWriteDirective(pageContent:Option[PageContent]): Array[String] = {
    pageContent.flatMap(_.write).getOrElse(AhaWikiConfig().permission.default.write()).split("""\s*,\s*""")
  }

  def isReadable(pageContent:Option[PageContent]): Boolean = {
    allowed(getReadDirective(pageContent))
  }

  def isReadable(pageContent:PageContent): Boolean = {
    allowed(getReadDirective(Some(pageContent)))
  }

  def isWritable(pageContent:Option[PageContent]): Boolean = {
    allowed(getWriteDirective(pageContent))
  }

  def isWritable(pageContent:PageContent): Boolean = {
    allowed(getWriteDirective(Some(pageContent)))
  }

  def allowed(directive: Array[String]): Boolean = {
    val optionId: Option[String] = provider.getId
    allowed(optionId, directive)
  }

  def allowed(optionId: Option[String], directive: Array[String]): Boolean = {
    optionId match {
      case Some(id) => directive.exists(s => s == "all" || s == "login" || s == id)
      case None => directive.contains("all")
    }
  }
}
