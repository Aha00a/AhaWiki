package logics

import com.aha00a.commons.Implicits._
import models.tables.Permission

class PermissionLogic(seqPermission: Seq[Permission]) {
  val seq: Seq[Permission] = seqPermission.sortBy(-_.priority)
  def permitted(target: String, actor: String, action: Int): Boolean = {
    val optionPermission = seq.find(_.matches(target, actor))
    optionPermission.exists(_.permitted(action))
  }
}
