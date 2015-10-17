package logics.wikis

import logics.{ApplicationConf, SessionLogic}
import models.PageContent
import play.api.mvc.{AnyContent, Request}

object WikiPermission {
  def getReadDirective(pageContent:PageContent): Array[String] = {
    pageContent.read.getOrElse(ApplicationConf.AhaWiki.config.permission.default.read).split("""\s*,\s*""")
  }

  def getWriteDirective(pageContent:PageContent): Array[String] = {
    pageContent.write.getOrElse(ApplicationConf.AhaWiki.config.permission.default.write).split("""\s*,\s*""")
  }

  def isReadable(pageContent:PageContent)(implicit request: Request[AnyContent]): Boolean = {
    allowed(getReadDirective(pageContent))
  }

  def isWritable(pageContent:PageContent)(implicit request: Request[AnyContent]): Boolean = {
    allowed(getWriteDirective(pageContent))
  }

  def allowed(directive: Array[String])(implicit request: Request[AnyContent]): Boolean = {
    SessionLogic.getId(request) match {
      case Some(id) => directive.exists(s => s == "all" || s == "login" || s == id)
      case None => directive.contains("all")
    }
  }
}
