package logics.wikis

import logics.AhaWikiCacheMemoryPermission
import logics.PermissionLogic
import models.ContextSite
import models.PageContent
import models.RequestWrapper
import models.tables.Permission
import models.tables.UserEmail

import java.sql.Connection

object WikiPermission {
  def apply()(implicit provider: RequestWrapper, connection: Connection, contextSite: ContextSite): WikiPermission = {
    val actors = provider.getUser
      .map { user =>
        val emails = UserEmail.selectEmailsByUser(user.seq)
        if (emails.nonEmpty) emails else user.loginEmail.toSeq
      }
      .getOrElse(Seq.empty)
    new WikiPermission(
      permissionLogic = new PermissionLogic(AhaWikiCacheMemoryPermission.get()(connection, contextSite.site)),
      actorProvider = () => actors,
    )
  }

  def fromRows(permissions: Seq[Permission], actor: String = ""): WikiPermission = {
    fromRows(permissions, if (actor.isEmpty) Seq.empty else Seq(actor))
  }

  def fromRows(permissions: Seq[Permission], actors: Seq[String]): WikiPermission = {
    new WikiPermission(
      permissionLogic = new PermissionLogic(permissions),
      actorProvider = () => actors,
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

  def actionLabel(action: Int): String =
    Permission.Action.values.find(_.id == action).map(_.toString).getOrElse(action.toString)
}

class WikiPermission private(permissionLogic: PermissionLogic, actorProvider: () => Seq[String]) {

  private def actors: Seq[String] = actorProvider()
  private def actorLabel: String = actors.mkString(", ")

  def isReadable(target: String, pageContent: Option[PageContent]): Boolean = {
    isReadable(target)
  }

  def isReadable(target: String, pageContent: PageContent): Boolean = {
    isReadable(target)
  }

  def isReadable(target: String): Boolean = {
    permissionLogic.permitted(target, actors, Permission.Action.Read.id)
  }

  def isReadableByAnonymous(target: String, pageContent: Option[PageContent]): Boolean = {
    isReadableByAnonymous(target)
  }

  def isReadableByAnonymous(target: String): Boolean = {
    permissionLogic.permitted(target, "", Permission.Action.Read.id)
  }

  def isWritable(target: String, pageContent: Option[PageContent]): Boolean = {
    val action = pageContent.map(_ => Permission.Action.Edit.id).getOrElse(Permission.Action.Create.id)
    permissionLogic.permitted(target, actors, action)
  }

  def isWritable(target: String, pageContent: PageContent): Boolean = {
    isWritable(target, Some(pageContent))
  }

  def isUploadable(target: String): Boolean = {
    permissionLogic.permitted(target, actors, Permission.Action.Upload.id)
  }

  def isRenamable(target: String): Boolean = {
    permissionLogic.permitted(target, actors, Permission.Action.Rename.id)
  }

  def isDeletable(target: String): Boolean = {
    permissionLogic.permitted(target, actors, Permission.Action.Delete.id)
  }

  def detail(target: String, pageContent: Option[PageContent]): WikiPermissionDetail = {
    val currentActors = actors
    val writeAction = pageContent.map(_ => Permission.Action.Edit.id).getOrElse(Permission.Action.Create.id)

    val anonymousReadPermission = permissionLogic.matched(target, "")
    val currentReadPermission = permissionLogic.matched(target, currentActors)
    val currentWritePermission = permissionLogic.matched(target, currentActors)

    WikiPermissionDetail(
      actor = actorLabel,
      anonymousReadPermission = anonymousReadPermission,
      currentReadPermission = currentReadPermission,
      currentWritePermission = currentWritePermission,
      anonymousReadable = anonymousReadPermission.exists(_.permitted(Permission.Action.Read.id)),
      currentReadable = currentReadPermission.exists(_.permitted(Permission.Action.Read.id)),
      currentWritable = currentWritePermission.exists(_.permitted(writeAction)),
    )
  }
}
