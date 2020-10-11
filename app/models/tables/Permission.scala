package models.tables

import java.sql.Connection

import com.aha00a.commons.Implicits._

case class Permission(seq: Int, priority: Int, target: String, actor: String, action: Int) {
  lazy val actorLevel: Int = actor match {
    case "" => 1
    case _ if(!actor.contains("@")) => 2
    case _ => 3
  }
  def matches(target: String, actor: String): Boolean = {
    if(this.target.isNotNullOrEmpty) {
      if(!this.target.r.matches(target))
        return false;
    }

    if(this.actor.isNotNullOrEmpty) {
      if(!this.actor.r.matches(actor))
        return false;
    }

    //noinspection RemoveRedundantReturn
    return true;
  }

  def permitted(action: Int): Boolean = this.action >= action

  def toTsvString: String = {
     Permission.unapply(this).map(_.productIterator.map(_.toString).mkString("\t")).getOrElse("")
  }
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
  def tupled = ((seq: Int, priority: Int, target: _root_.scala.Predef.String, actor: _root_.scala.Predef.String, action: Int) => apply(seq, priority, target, actor, action)).tupled

  def selectWhereValue(value: String)(implicit connection: Connection): List[Permission] = {
    List()
  }
}
