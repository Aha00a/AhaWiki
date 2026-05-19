package logics.wikis

import logics.AhaWikiCacheMemoryPermission
import logics.PermissionLogic
import models.ContextSite
import models.PageContent
import models.RequestWrapper
import models.tables.Permission

import java.sql.Connection

object WikiPermission {
  def apply()(implicit provider: RequestWrapper, connection: Connection, contextSite: ContextSite): WikiPermission = {
    new WikiPermission(
      permissionLogic = new PermissionLogic(AhaWikiCacheMemoryPermission.get()(connection, contextSite.site)),
      actorProvider = () => provider.getUser.map(_.email).getOrElse(""),
    )
  }

  def fromRows(permissions: Seq[Permission], actor: String = ""): WikiPermission = {
    new WikiPermission(
      permissionLogic = new PermissionLogic(permissions),
      actorProvider = () => actor,
    )
  }
}

case class WikiPermissionDetail(
  actor: String,
  anonymousReadPermission: Option[Permission],
  currentReadPermission: Option[Permission],
  currentWritePermission: Option[Permission],
  anonymousReadable: Boolean,
  currentReadable: Boolean,
  currentWritable: Boolean,
) {
  def actorLabel: String = if (actor.isEmpty) "Anonymous" else actor

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
    s"${permission.targetType}($target), ${permission.actorType}($actor), ${actionLabel(permission.action)}"
  }

  def actionLabel(action: Int): String = action match {
    case Permission.none => "None"
    case Permission.read => "Read"
    case Permission.edit => "Edit"
    case Permission.create => "Create"
    case Permission.upload => "Upload"
    case Permission.delete => "Delete"
    case Permission.admin => "Admin"
    case _ => action.toString
  }
}

class WikiPermission private(permissionLogic: PermissionLogic, actorProvider: () => String) {

  private def actor: String = actorProvider()

  def isReadable(target: String, pageContent: Option[PageContent]): Boolean = {
    isReadable(target)
  }

  def isReadable(target: String, pageContent: PageContent): Boolean = {
    isReadable(target)
  }

  def isReadable(target: String): Boolean = {
    permissionLogic.permitted(target, actor, Permission.read)
  }

  def isReadableByAnonymous(target: String, pageContent: Option[PageContent]): Boolean = {
    isReadableByAnonymous(target)
  }

  def isReadableByAnonymous(target: String): Boolean = {
    permissionLogic.permitted(target, "", Permission.read)
  }

  def isWritable(target: String, pageContent: Option[PageContent]): Boolean = {
    val action = pageContent.map(_ => Permission.edit).getOrElse(Permission.create)
    permissionLogic.permitted(target, actor, action)
  }

  def isWritable(target: String, pageContent: PageContent): Boolean = {
    isWritable(target, Some(pageContent))
  }

  def detail(target: String, pageContent: Option[PageContent]): WikiPermissionDetail = {
    val currentActor = actor
    val writeAction = pageContent.map(_ => Permission.edit).getOrElse(Permission.create)

    val anonymousReadPermission = permissionLogic.matched(target, "")
    val currentReadPermission = permissionLogic.matched(target, currentActor)
    val currentWritePermission = permissionLogic.matched(target, currentActor)

    WikiPermissionDetail(
      actor = currentActor,
      anonymousReadPermission = anonymousReadPermission,
      currentReadPermission = currentReadPermission,
      currentWritePermission = currentWritePermission,
      anonymousReadable = anonymousReadPermission.exists(_.permitted(Permission.read)),
      currentReadable = currentReadPermission.exists(_.permitted(Permission.read)),
      currentWritable = currentWritePermission.exists(_.permitted(writeAction)),
    )
  }
}
