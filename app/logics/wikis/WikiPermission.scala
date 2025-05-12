package logics.wikis

import logics.AhaWikiConfig
import models.RequestWrapper
import models.PageContent
import models.tables.Site

import java.sql.Connection

object WikiPermission {
  def apply()(implicit provider: RequestWrapper, connection: Connection, site: Site): WikiPermission = new WikiPermission()
}

class WikiPermission(implicit provider: RequestWrapper, connection: Connection, site: Site) {
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
      case Some(id) => directive.exists(s => s == "all" || s == "login" || s == id || (s.startsWith("@") && id.endsWith(s)))
      case None => directive.contains("all")
    }
  }
}
