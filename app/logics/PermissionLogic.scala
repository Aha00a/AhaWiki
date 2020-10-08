package logics

import com.aha00a.commons.Implicits._
import models.tables.Permission

class PermissionLogic(seqPermission: Seq[Permission]) {
  val seq: Seq[Permission] = seqPermission.sortBy(-_.priority)

  def permitted(target: String, actor: String, action: Int): Boolean = {
    val optionPermission = seq.find(_.matches(target, actor))
    optionPermission.exists(_.permitted(action))
  }

  def toTsvString: String = seq.map(_.toTsvString).mkString("\n")
  def toLogString: String = Seq(
    "=" * 80,
    toTsvString,
    "=" * 80
  ).mkString("\n")
}
