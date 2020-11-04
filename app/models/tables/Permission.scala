package models.tables

import java.sql.Connection

import anorm.SqlParser._
import anorm._
import com.aha00a.commons.Implicits._

case class Permission(seq: Int, target: String, actor: String, action: Int) {
  lazy val targetLevel: Int = target match {
    case "" => 1
    case _ if target.endsWith("?") => 2
    case _ => 3
  }

  lazy val actorLevel: Int = actor match {
    case "" => 1
    case _ if actor.startsWith("@") => 2
    case _ => 3
  }

  lazy val priority: Int = targetLevel + actorLevel

  def matches(target: String, actor: String): Boolean = {
    if(this.target.isNotNullOrEmpty) {
      if(this.target.endsWith("?")) {
        if(!target.startsWith(this.target.substring(0, this.target.length - 1))) {
          return false
        }
      } else if(this.target != target) {
        return false
      }
    }

    if(this.actor.isNotNullOrEmpty) {
      if(this.actor.startsWith("@")) {
        if(!actor.endsWith(this.actor)){
          return false
        }
      } else if(this.actor != actor) {
        return false
      }
    }

    //noinspection RemoveRedundantReturn
    return true
  }

  def permitted(action: Int): Boolean = this.action >= action

  def toTsvString: String = Seq(seq, target, actor, action).map(_.toString).mkString("\t")

  def toDebugString: String = Seq(seq, priority, target, targetLevel, actor, actorLevel, action).map(_.toString.padRight(25)).mkString(" | ")
}

object Permission {
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


  //noinspection TypeAnnotation
  def tupled = (apply _).tupled

  def select()(implicit connection: Connection): List[Permission] = {
    SQL"SELECT seq, target, actor, action FROM Permission"
      .as(int("seq") ~ str("target") ~ str("actor") ~ int("action") *).map(flatten)
      .map(Permission.tupled)
  }
}
