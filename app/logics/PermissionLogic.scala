package logics

import com.aha00a.commons.Implicits._
import models.tables.Permission

class PermissionLogic(seqPermission: Seq[Permission]) {
  val seq: Seq[Permission] = seqPermission.sortBy(-_.priority)
  def check(target: String, actor: String, action: Int): Boolean = {
    seq.find(_.matches(target, actor)).exists(_.permitted(action))
  }
}
