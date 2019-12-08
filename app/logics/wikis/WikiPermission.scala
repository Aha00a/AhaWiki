package logics.wikis

import logics.{AhaWikiConfig, ApplicationConf, SessionLogic}
import models.{PageContent, WikiContext}
import play.api.cache.CacheApi
import play.api.db.Database
import play.api.mvc.Request

object WikiPermission {
  def getReadDirective(pageContent:Option[PageContent])(implicit request:Request[Any], cacheApi: CacheApi, db:Database): Array[String] = {
    pageContent.flatMap(_.read).getOrElse(AhaWikiConfig.permission.default.read()).split("""\s*,\s*""")
  }

  def getWriteDirective(pageContent:Option[PageContent])(implicit request:Request[Any], cacheApi: CacheApi, db:Database): Array[String] = {
    pageContent.flatMap(_.write).getOrElse(AhaWikiConfig.permission.default.write()).split("""\s*,\s*""")
  }

  def isReadable(pageContent:Option[PageContent])(implicit request:Request[Any], cacheApi: CacheApi, db:Database): Boolean = {
    allowed(getReadDirective(pageContent))
  }

  def isReadable(pageContent:PageContent)(implicit request:Request[Any], cacheApi: CacheApi, db:Database): Boolean = {
    allowed(getReadDirective(Some(pageContent)))
  }

  def isWritable(pageContent:Option[PageContent])(implicit request:Request[Any], cacheApi: CacheApi, db:Database): Boolean = {
    allowed(getWriteDirective(pageContent))
  }

  def isWritable(pageContent:PageContent)(implicit request:Request[Any], cacheApi: CacheApi, db:Database): Boolean = {
    allowed(getWriteDirective(Some(pageContent)))
  }

  def allowed(directive: Array[String])(implicit request:Request[Any], cacheApi: CacheApi, db:Database): Boolean = {
    SessionLogic.getId(request) match {
      case Some(id) => directive.exists(s => s == "all" || s == "login" || s == id)
      case None => directive.contains("all")
    }
  }
}
