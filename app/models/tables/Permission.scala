package models.tables

import java.sql.Connection

import anorm.SqlParser._
import anorm._
import com.aha00a.commons.Implicits._

case class Permission(target: String, targetType: Permission.TargetType.Value, actor: String, actorType: Permission.ActorType.Value, action: Int) {
  lazy val targetLevel: Int = targetType match {
    case Permission.TargetType.All => 1
    case Permission.TargetType.StartsWith | Permission.TargetType.EndsWith => 2
    case Permission.TargetType.Exact => 3
  }

  lazy val actorLevel: Int = actorType match {
    case Permission.ActorType.All => 1
    case Permission.ActorType.Login | Permission.ActorType.Domain => 2
    case Permission.ActorType.Exact => 3
  }

  lazy val specificity: Int = targetLevel + actorLevel

  def matches(target: String, actor: String): Boolean = {
    targetType match {
      case Permission.TargetType.All =>
      case Permission.TargetType.Exact =>
        if (this.target != target) return false
      case Permission.TargetType.StartsWith =>
        if (!target.startsWith(this.target)) return false
      case Permission.TargetType.EndsWith =>
        if (!target.endsWith(this.target)) return false
    }

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

  def permitted(action: Int): Boolean = this.action >= action

  def toTsvString: String = Seq(targetType, target, actorType, actor, action).map(_.toString).mkString("\t")

  def toDebugString: String = Seq(specificity, targetType, target, targetLevel, actorType, actor, actorLevel, action).map(_.toString.padRight(25)).mkString(" | ")
}

object Permission {
  object TargetType extends Enumeration {
    val All, Exact, StartsWith, EndsWith = Value
  }

  object ActorType extends Enumeration {
    val All, Login, Exact, Domain = Value
  }

  // TODO: consider enum
  val none = 0
  val read = 1
  val edit = 2
  val create = 4
  val upload = 8
  val delete = 16
  val admin = 255

//  object Action extends Enumeration {
//    type Action = Value;
//    val none: Action = Value(0)
//    val read: Action = Value(1)
//    val edit: Action = Value(2)
//    val create: Action = Value(4)
//    val upload: Action = Value(8)
//    val delete: Action = Value(16)
//    val admin: Action = Value(255)
//  }


  def fromRow(row: (String, TargetType.Value, String, ActorType.Value, Int)): Permission = {
    Permission(row._1, row._2, row._3, row._4, row._5)
  }

  def apply(target: String, actor: String, action: Int): Permission = {
    Permission(
      normalizeLegacyTarget(target),
      inferLegacyTargetType(target),
      normalizeLegacyActor(actor),
      inferLegacyActorType(actor),
      action,
    )
  }

  private def normalizeLegacyTarget(target: String): String = {
    if (target.endsWith("?")) target.substring(0, target.length - 1) else target
  }

  private def inferLegacyTargetType(target: String): TargetType.Value = {
    target match {
      case "" => TargetType.All
      case _ if target.endsWith("?") => TargetType.StartsWith
      case _ => TargetType.Exact
    }
  }

  private def normalizeLegacyActor(actor: String): String = {
    if (actor == "login") "" else actor
  }

  private def inferLegacyActorType(actor: String): ActorType.Value = {
    actor match {
      case "" => ActorType.All
      case "login" => ActorType.Login
      case _ if actor.startsWith("@") => ActorType.Domain
      case _ => ActorType.Exact
    }
  }

  private val targetTypeParser: RowParser[TargetType.Value] = str("targetType").map(TargetType.withName)
  private val actorTypeParser: RowParser[ActorType.Value] = str("actorType").map(ActorType.withName)

  def select()(implicit connection: Connection, site: Site): List[Permission] = {
    SQL"SELECT target, targetType, actor, actorType, action FROM Permission WHERE site = ${site.seq}"
      .as(str("target") ~ targetTypeParser ~ str("actor") ~ actorTypeParser ~ int("action") *).map(flatten)
      .map(Permission.fromRow)
  }
}
