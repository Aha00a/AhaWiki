package logics.wikis

import logics.AhaWikiConfig
import logics.PermissionLogic
import models.ContextSite
import models.PageContent
import models.RequestWrapper
import models.tables.Permission

import java.sql.Connection

object WikiPermission {
  def apply()(implicit provider: RequestWrapper, connection: Connection, contextSite: ContextSite): WikiPermission = new WikiPermission()
}

class WikiPermission(implicit provider: RequestWrapper, connection: Connection, contextSite: ContextSite) {
  private lazy val permissionLogic = new PermissionLogic(Permission.select()(connection, contextSite.site))

  private def actor: String = provider.getUser.map(_.email).getOrElse("")

  def getReadDirective(pageContent: Option[PageContent]): Array[String] = {
    pageContent.flatMap(_.read).getOrElse(AhaWikiConfig().permission.default.read()).split("""\s*,\s*""")
  }

  def getWriteDirective(pageContent: Option[PageContent]): Array[String] = {
    pageContent.flatMap(_.write).getOrElse(AhaWikiConfig().permission.default.write()).split("""\s*,\s*""")
  }

  def isReadable(target: String, pageContent: Option[PageContent]): Boolean = {
    permissionLogic
      .permittedOption(target, actor, Permission.read)
      .getOrElse(allowed(getReadDirective(pageContent)))
  }

  def isReadable(target: String, pageContent: PageContent): Boolean = {
    isReadable(target, Some(pageContent))
  }

  def isReadable(target: String, readDirective: Array[String]): Boolean = {
    permissionLogic
      .permittedOption(target, actor, Permission.read)
      .getOrElse(allowed(readDirective))
  }

  def isReadableByAnonymous(target: String, pageContent: Option[PageContent]): Boolean = {
    permissionLogic
      .permittedOption(target, "", Permission.read)
      .getOrElse(allowed(None, getReadDirective(pageContent)))
  }

  def isWritable(target: String, pageContent: Option[PageContent]): Boolean = {
    val action = pageContent.map(_ => Permission.edit).getOrElse(Permission.create)
    permissionLogic
      .permittedOption(target, actor, action)
      .getOrElse(allowed(getWriteDirective(pageContent)))
  }

  def isWritable(target: String, pageContent: PageContent): Boolean = {
    isWritable(target, Some(pageContent))
  }

  @deprecated("Pass the page target so Permission table rows can be applied.", "Permission table")
  def isReadable(pageContent: Option[PageContent]): Boolean = {
    allowed(getReadDirective(pageContent))
  }

  @deprecated("Pass the page target so Permission table rows can be applied.", "Permission table")
  def isReadable(pageContent: PageContent): Boolean = {
    allowed(getReadDirective(Some(pageContent)))
  }

  @deprecated("Pass the page target so Permission table rows can be applied.", "Permission table")
  def isWritable(pageContent: Option[PageContent]): Boolean = {
    allowed(getWriteDirective(pageContent))
  }

  @deprecated("Pass the page target so Permission table rows can be applied.", "Permission table")
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
