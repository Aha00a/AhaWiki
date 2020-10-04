package logics

import com.aha00a.commons.Implicits._
import models.tables.Permission

class PermissionLogic(seqPermission: Seq[Permission]) {
  def check(target: String, actor: String, action: Int): Boolean = {
    seqPermission.find(_.matches(target, actor)).exists(_.permitted(action))
  }
}
