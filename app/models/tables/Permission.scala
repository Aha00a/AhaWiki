package models.tables

import java.sql.Connection

import anorm.SqlParser._
import anorm._
import com.aha00a.commons.Implicits._

import scala.util.Try

case class Permission(target: String, targetType: Permission.TargetType.Value, actor: String, actorType: Permission.ActorType.Value, action: Int) {
  private lazy val targetRegularExpressionPattern = Try(this.target.r.pattern).toOption

  lazy val targetLevel: Int = targetType match {
    case Permission.TargetType.All => 1
    case Permission.TargetType.StartsWith | Permission.TargetType.EndsWith | Permission.TargetType.RegularExpression => 2
    case Permission.TargetType.Exact => 3
  }

  lazy val actorLevel: Int = actorType match {
    case Permission.ActorType.All => 1
    case Permission.ActorType.Login | Permission.ActorType.Domain => 2
    case Permission.ActorType.Exact => 3
  }

  lazy val specificity: Int = targetLevel + actorLevel

  def matches(target: String, actor: String): Boolean = {
    if (!targetMatches(target)) return false

    actorType match {
      case Permission.ActorType.All =>
      case Permission.ActorType.Login =>
        if (actor.isNullOrEmpty) return false
      case Permission.ActorType.Exact =>
        if (this.actor != actor) return false
      case Permission.ActorType.Domain =>
        if (!actor.endsWith(this.actor)) return false
    }

    //noinspection RemoveRedundantReturn
    return true
  }

  def targetMatches(target: String): Boolean = {
    targetType match {
      case Permission.TargetType.All =>
      case Permission.TargetType.Exact =>
        if (this.target != target) return false
      case Permission.TargetType.StartsWith =>
        if (!target.startsWith(this.target)) return false
      case Permission.TargetType.EndsWith =>
        if (!target.endsWith(this.target)) return false
      case Permission.TargetType.RegularExpression =>
        // Matcher.matches enforces a full target match, so Permission regex rows do not need ^...$ anchors.
        if (!targetRegularExpressionPattern.exists(_.matcher(target).matches())) return false
    }

    //noinspection RemoveRedundantReturn
    return true
  }

  def permitted(action: Int): Boolean = this.action >= action

  def toTsvString: String = Seq(targetType, target, actorType, actor, action).map(_.toString).mkString("\t")

  def toDebugString: String = Seq(specificity, targetType, target, targetLevel, actorType, actor, actorLevel, action).map(_.toString.padRight(25)).mkString(" | ")
}

object Permission {
  object TargetType extends Enumeration {
    val All, Exact, StartsWith, EndsWith, RegularExpression = Value
  }

  object ActorType extends Enumeration {
    val All, Login, Exact, Domain = Value
  }

  object Action extends Enumeration {
    type Action = Value
    val None: Action = Value(0, "None")
    val Read: Action = Value(1, "Read")
    val Edit: Action = Value(2, "Edit")
    val Create: Action = Value(4, "Create")
    val Upload: Action = Value(8, "Upload")
    val Delete: Action = Value(16, "Delete")
    val Admin: Action = Value(255, "Admin")
  }

  def fromRow(row: (String, TargetType.Value, String, ActorType.Value, Int)): Permission = {
    Permission(row._1, row._2, row._3, row._4, row._5)
  }

  private val targetTypeParser: RowParser[TargetType.Value] = str("targetType").map { value =>
    parseTargetType(value).fold(message => throw new IllegalArgumentException(message), identity)
  }
  private val actorTypeParser: RowParser[ActorType.Value] = str("actorType").map { value =>
    parseActorType(value).fold(message => throw new IllegalArgumentException(message), identity)
  }

  def parseTargetType(value: String): Either[String, TargetType.Value] = {
    TargetType.values.find(_.toString == value.trim).toRight(s"targetType must be one of: ${TargetType.values.mkString(", ")}")
  }

  def validate(permission: Permission): Either[String, Permission] = {
    if (permission.targetType == TargetType.RegularExpression) {
      Try(permission.target.r).toEither.left.map(error => s"target regular expression is invalid: ${error.getMessage}").map(_ => permission)
    } else {
      Right(permission)
    }
  }

  def parseActorType(value: String): Either[String, ActorType.Value] = {
    ActorType.values.find(_.toString == value.trim).toRight(s"actorType must be one of: ${ActorType.values.mkString(", ")}")
  }

  def parseAction(value: String): Either[String, Int] = {
    val normalized = value.trim
    Action.values.find(_.toString == normalized).map(_.id)
      .orElse(scala.util.Try(normalized.toInt).toOption.filter(Action.values.map(_.id).contains))
      .toRight(s"action must be one of: ${Action.values.map(a => s"${a.toString}(${a.id})").mkString(", ")}")
  }

  def select()(implicit connection: Connection, site: Site): List[Permission] = {
    SQL"SELECT target, targetType, actor, actorType, action FROM Permission WHERE site = ${site.seq}"
      .as(str("target") ~ targetTypeParser ~ str("actor") ~ actorTypeParser ~ int("action") *).map(flatten)
      .map(Permission.fromRow)
  }

  def upsert(permission: Permission)(implicit connection: Connection, site: Site): Int = {
    SQL"""
      INSERT INTO Permission (site, target, targetType, actor, actorType, action)
      VALUES (${site.seq}, ${permission.target}, ${permission.targetType.toString}, ${permission.actor}, ${permission.actorType.toString}, ${permission.action})
      ON DUPLICATE KEY UPDATE
        action = VALUES(action),
        dateUpdated = CURRENT_TIMESTAMP
    """.executeUpdate()
  }

  def delete(permission: Permission)(implicit connection: Connection, site: Site): Int = {
    SQL"""
      DELETE FROM Permission
      WHERE site = ${site.seq}
        AND target = ${permission.target}
        AND targetType = ${permission.targetType.toString}
        AND actor = ${permission.actor}
        AND actorType = ${permission.actorType.toString}
    """.executeUpdate()
  }

  def deleteExactTarget(target: String)(implicit connection: Connection, site: Site): Int = {
    SQL"""
      DELETE FROM Permission
      WHERE site = ${site.seq}
        AND target = $target
        AND targetType = ${TargetType.Exact.toString}
    """.executeUpdate()
  }

  case class SiteAudit(siteSeq: Long, siteName: String, permissionCount: Long, publicReadRows: Long, loginCreateRows: Long)

  private val siteAuditParser =
    long("siteSeq") ~ str("siteName") ~ long("permissionCount") ~ long("publicReadRows") ~ long("loginCreateRows") map {
      case siteSeq ~ siteName ~ permissionCount ~ publicReadRows ~ loginCreateRows =>
        SiteAudit(siteSeq, siteName, permissionCount, publicReadRows, loginCreateRows)
    }

  def auditSites()(implicit connection: Connection): Seq[SiteAudit] = {
    SQL"""
      SELECT
        S.seq AS siteSeq,
        S.name AS siteName,
        COUNT(P.site) AS permissionCount,
        COALESCE(SUM(CASE
          WHEN P.targetType = 'All' AND P.target = '' AND P.actorType = 'All' AND P.actor = '' AND P.action >= ${Action.Read.id} THEN 1
          ELSE 0
        END), 0) AS publicReadRows,
        COALESCE(SUM(CASE
          WHEN P.targetType = 'All' AND P.target = '' AND P.actorType = 'Login' AND P.actor = '' AND P.action >= ${Action.Create.id} THEN 1
          ELSE 0
        END), 0) AS loginCreateRows
      FROM Site S
      LEFT JOIN Permission P ON P.site = S.seq
      GROUP BY S.seq, S.name
      ORDER BY S.seq
    """.as(siteAuditParser.*)
  }
}
