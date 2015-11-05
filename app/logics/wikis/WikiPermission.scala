package logics.wikis

import logics.{ApplicationConf, SessionLogic}
import models.{PageContent, WikiContext}

object WikiPermission {
  def getReadDirective(pageContent:Option[PageContent])(implicit wikiContext: WikiContext): Array[String] = {
    pageContent.flatMap(_.read).getOrElse(ApplicationConf.AhaWiki.config.permission.default.read()).split("""\s*,\s*""")
  }

  def getWriteDirective(pageContent:Option[PageContent])(implicit wikiContext: WikiContext): Array[String] = {
    pageContent.flatMap(_.write).getOrElse(ApplicationConf.AhaWiki.config.permission.default.write()).split("""\s*,\s*""")
  }

  def isReadable(pageContent:Option[PageContent])(implicit wikiContext: WikiContext): Boolean = {
    allowed(getReadDirective(pageContent))
  }

  def isReadable(pageContent:PageContent)(implicit wikiContext: WikiContext): Boolean = {
    allowed(getReadDirective(Some(pageContent)))
  }

  def isWritable(pageContent:Option[PageContent])(implicit wikiContext: WikiContext): Boolean = {
    allowed(getWriteDirective(pageContent))
  }

  def isWritable(pageContent:PageContent)(implicit wikiContext: WikiContext): Boolean = {
    allowed(getWriteDirective(Some(pageContent)))
  }

  def allowed(directive: Array[String])(implicit wikiContext: WikiContext): Boolean = {
    SessionLogic.getId(wikiContext.request) match {
      case Some(id) => directive.exists(s => s == "all" || s == "login" || s == id)
      case None => directive.contains("all")
    }
  }
}
