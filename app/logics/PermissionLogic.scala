package logics

import com.aha00a.commons.Implicits._
import models.tables.Permission
import models.tables.Site
import play.api.db.Database

import java.sql.Connection

object PermissionLogic {

  /**
   * Whether an anonymous visitor may read a page on another site.
   *
   * Cross-site features — similar pages, twin pages, the sister-wiki listing — may only
   * reveal a page that a logged-out visitor could reach on its own site. Deciding that per
   * feature is how one of them ends up leaking a page the others hide, so the decision lives
   * here.
   */
  def anonymousCanRead(targetSite: Site, pageName: String)(implicit connection: Connection): Boolean = {
    val permissionLogic = new PermissionLogic(AhaWikiCacheMemoryPermission.get()(connection, targetSite))
    permissionLogic.permitted(pageName, "", Permission.Action.Read.id)
  }

  /** Same, for a caller that has the site's seq rather than the site. Unknown seq reads as no. */
  def anonymousCanRead(siteSeq: Long, pageName: String)(implicit connection: Connection, database: Database): Boolean =
    AhaWikiCacheMemoryDomainSite.getSite(siteSeq)(database).exists(anonymousCanRead(_, pageName))
}

class PermissionLogic(seqPermission: Seq[Permission]) {
  val seq: Seq[Permission] = seqPermission.sortBy(permission => (
    -permission.specificity,
    -permission.target.length,
    -permission.actor.length,
  ))

  def permitted(target: String, actor: String, action: Int): Boolean = {
    permittedOption(target, actor, action).getOrElse(false)
  }

  def permitted(target: String, actors: Seq[String], action: Int): Boolean = {
    permittedOption(target, actors, action).getOrElse(false)
  }

  def permittedOption(target: String, actor: String, action: Int): Option[Boolean] = {
    matched(target, actor).map(_.permitted(action))
  }

  def permittedOption(target: String, actors: Seq[String], action: Int): Option[Boolean] = {
    matched(target, actors).map(_.permitted(action))
  }

  def matched(target: String, actor: String): Option[Permission] = {
    seq.find(_.matches(target, actor))
  }

  def matched(target: String, actors: Seq[String]): Option[Permission] = {
    seq.find(_.matches(target, actors))
  }

  def toTsvString: String = seq.map(_.toTsvString).mkString("\n")

  def toDebugString: String = seq.map(_.toDebugString).mkString("\n")

  def toLogString(title: String): String = Seq(
    "=" * 200,
    title,
    "-" * 200,
    toDebugString,
    "-" * 200,
    toTsvString,
    "=" * 200,
  ).mkString("\n")
}
