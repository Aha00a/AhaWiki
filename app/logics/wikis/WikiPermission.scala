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

case class WikiPermissionDetail(
  actor: String,
  readDirective: Array[String],
  writeDirective: Array[String],
  anonymousReadPermission: Option[Permission],
  currentReadPermission: Option[Permission],
  currentWritePermission: Option[Permission],
  shebangAnonymousReadable: Boolean,
  shebangCurrentReadable: Boolean,
  shebangCurrentWritable: Boolean,
  tableAnonymousReadable: Option[Boolean],
  tableCurrentReadable: Option[Boolean],
  tableCurrentWritable: Option[Boolean],
) {
  def actorLabel: String = if (actor.isEmpty) "Anonymous" else actor

  def effectiveAnonymousReadable: Boolean = tableAnonymousReadable.getOrElse(shebangAnonymousReadable)

  def effectiveCurrentReadable: Boolean = tableCurrentReadable.getOrElse(shebangCurrentReadable)

  def effectiveCurrentWritable: Boolean = tableCurrentWritable.getOrElse(shebangCurrentWritable)

  def hasMismatch: Boolean = {
    tableAnonymousReadable.exists(_ != shebangAnonymousReadable) ||
      tableCurrentReadable.exists(_ != shebangCurrentReadable) ||
      tableCurrentWritable.exists(_ != shebangCurrentWritable)
  }

  def shebangReadSource: String = s"#!read ${readDirective.mkString(", ")}"

  def shebangWriteSource: String = s"#!write ${writeDirective.mkString(", ")}"

  def tableReadSource: String = currentReadPermission
    .map(WikiPermissionDetail.permissionLabel)
    .getOrElse("No matching Permission table row")

  def tableWriteSource: String = currentWritePermission
    .map(WikiPermissionDetail.permissionLabel)
    .getOrElse("No matching Permission table row")

  def tableAnonymousReadSource: String = anonymousReadPermission
    .map(WikiPermissionDetail.permissionLabel)
    .getOrElse("No matching Permission table row")
}

object WikiPermissionDetail {
  def permissionLabel(permission: Permission): String = {
    val target = if (permission.target.isEmpty) "*" else permission.target
    val actor = if (permission.actor.isEmpty) "*" else permission.actor
    s"Permission table: target=${permission.targetType}($target), actor=${permission.actorType}($actor), action=${actionLabel(permission.action)}"
  }

  def actionLabel(action: Int): String = action match {
    case Permission.none => "none"
    case Permission.read => "read"
    case Permission.edit => "edit"
    case Permission.create => "create"
    case Permission.upload => "upload"
    case Permission.delete => "delete"
    case Permission.admin => "admin"
    case _ => action.toString
  }
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

  def detail(target: String, pageContent: Option[PageContent]): WikiPermissionDetail = {
    val currentActor = actor
    val readDirective = getReadDirective(pageContent)
    val writeDirective = getWriteDirective(pageContent)
    val writeAction = pageContent.map(_ => Permission.edit).getOrElse(Permission.create)

    val anonymousReadPermission = permissionLogic.matched(target, "")
    val currentReadPermission = permissionLogic.matched(target, currentActor)
    val currentWritePermission = permissionLogic.matched(target, currentActor)

    WikiPermissionDetail(
      actor = currentActor,
      readDirective = readDirective,
      writeDirective = writeDirective,
      anonymousReadPermission = anonymousReadPermission,
      currentReadPermission = currentReadPermission,
      currentWritePermission = currentWritePermission,
      shebangAnonymousReadable = allowed(None, readDirective),
      shebangCurrentReadable = allowed(readDirective),
      shebangCurrentWritable = allowed(writeDirective),
      tableAnonymousReadable = anonymousReadPermission.map(_.permitted(Permission.read)),
      tableCurrentReadable = currentReadPermission.map(_.permitted(Permission.read)),
      tableCurrentWritable = currentWritePermission.map(_.permitted(writeAction)),
    )
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
