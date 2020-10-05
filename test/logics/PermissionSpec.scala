package logics

import org.scalatest.freespec.AnyFreeSpec
import models.tables.Permission

class PermissionSpec extends AnyFreeSpec {

  import models.tables.Permission
  var priority = 10000
  def decrPriority: Int = {
    priority -= 1
    priority
  }

  val targetPrivate = "Private"
  val actorSomeone = "someone@example.com"
  val actorAha00a = "aha00a@gmail.com"

  "check" - {

    "anonymous" in {
      val permissionLogic = new PermissionLogic(Seq(
        Permission(5, decrPriority, targetPrivate + ".*", "aha00a@.+", Permission.admin),
        Permission(4, decrPriority, targetPrivate + ".*", "", Permission.none),

        Permission(3, decrPriority, "", "aha00a@.+", Permission.admin),
        Permission(2, decrPriority, "", ".+", Permission.edit),
        Permission(1, decrPriority, "", "", Permission.read),
      ))

      val seqAction = Seq(
        Permission.read,
        Permission.edit,
        Permission.create,
        Permission.upload,
        Permission.delete,
      )

      assert(seqAction.map(a => permissionLogic.permitted("" , ""          , a)) === "10000".map(_ == '1'))
      assert(seqAction.map(a => permissionLogic.permitted("" , actorSomeone, a)) === "11000".map(_ == '1'))
      assert(seqAction.map(a => permissionLogic.permitted("" , actorAha00a , a)) === "11111".map(_ == '1'))

      assert(seqAction.map(a => permissionLogic.permitted("a", ""          , a)) === "10000".map(_ == '1'))
      assert(seqAction.map(a => permissionLogic.permitted("a", actorSomeone, a)) === "11000".map(_ == '1'))
      assert(seqAction.map(a => permissionLogic.permitted("a", actorAha00a , a)) === "11111".map(_ == '1'))

      assert(seqAction.map(a => permissionLogic.permitted(targetPrivate, ""          , a)) === "00000".map(_ == '1'))
      assert(seqAction.map(a => permissionLogic.permitted(targetPrivate, actorSomeone, a)) === "00000".map(_ == '1'))
      assert(seqAction.map(a => permissionLogic.permitted(targetPrivate, actorAha00a , a)) === "11111".map(_ == '1'))
    }
  }
}
