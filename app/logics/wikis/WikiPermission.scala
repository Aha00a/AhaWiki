package logics.wikis

import logics.AhaWikiConfig
import models.ContextSite
import models.PageContent
import models.RequestWrapper

import java.sql.Connection

object WikiPermission {
  def apply()(implicit provider: RequestWrapper, connection: Connection, contextSite: ContextSite): WikiPermission = new WikiPermission()
}

class WikiPermission(implicit provider: RequestWrapper, connection: Connection, contextSite: ContextSite) {
  def getReadDirective(pageContent: Option[PageContent]): Array[String] = {
    pageContent.flatMap(_.read).getOrElse(AhaWikiConfig().permission.default.read()).split("""\s*,\s*""")
  }

  def getWriteDirective(pageContent: Option[PageContent]): Array[String] = {
    pageContent.flatMap(_.write).getOrElse(AhaWikiConfig().permission.default.write()).split("""\s*,\s*""")
  }

  def isReadable(pageContent: Option[PageContent]): Boolean = {
    allowed(getReadDirective(pageContent))
  }

  def isReadable(pageContent: PageContent): Boolean = {
    allowed(getReadDirective(Some(pageContent)))
  }

  def isWritable(pageContent: Option[PageContent]): Boolean = {
    allowed(getWriteDirective(pageContent))
  }

  def isWritable(pageContent: PageContent): Boolean = {
    allowed(getWriteDirective(Some(pageContent)))
  }

  def allowed(directive: Array[String]): Boolean = {
    val optionEmail: Option[String] = provider.getUser.map(_.email)
    allowed(optionEmail, directive)
  }

  def allowed(optionEmail: Option[String], directive: Array[String]): Boolean = {
    optionEmail match {
      case Some(id) => directive.exists(s => s == "all" || s == "login" || s == id || (s.startsWith("@") && id.endsWith(s)))
      case None => directive.contains("all")
    }
  }
}
