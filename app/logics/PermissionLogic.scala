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

  def permittedOption(target: String, actor: String, action: Int): Option[Boolean] = {
    val optionPermission = seq.find(_.matches(target, actor))
    optionPermission.map(_.permitted(action))
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
