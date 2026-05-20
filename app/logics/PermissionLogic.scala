package logics

import com.aha00a.commons.Implicits._
import models.tables.Permission

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
