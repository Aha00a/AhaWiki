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

    "Open" in {
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

      assert(seqAction.map(a => permissionLogic.permitted("FrontPage", ""          , a)) === "10000".map(_ == '1'))
      assert(seqAction.map(a => permissionLogic.permitted("FrontPage", actorSomeone, a)) === "11000".map(_ == '1'))
      assert(seqAction.map(a => permissionLogic.permitted("FrontPage", actorAha00a , a)) === "11111".map(_ == '1'))

      assert(seqAction.map(a => permissionLogic.permitted(targetPrivate, ""          , a)) === "00000".map(_ == '1'))
      assert(seqAction.map(a => permissionLogic.permitted(targetPrivate, actorSomeone, a)) === "00000".map(_ == '1'))
      assert(seqAction.map(a => permissionLogic.permitted(targetPrivate, actorAha00a , a)) === "11111".map(_ == '1'))
    }

    "Private" in {
      val permissionLogic = new PermissionLogic(Seq(
        Permission(1, decrPriority, "FrontPage", "aha00a@gmail.com", Permission.admin),
        Permission(1, decrPriority, "FrontPage", "", Permission.read),
        Permission(1, decrPriority, "Memo", "aha00a@gmail.com", Permission.admin),
        Permission(1, decrPriority, "Memo", "", Permission.read),
        Permission(1, decrPriority, "", "aha00a@gmail.com", Permission.admin),
        Permission(1, decrPriority, "", "", Permission.none),
      ))

      val seqAction = Seq(
        Permission.read,
        Permission.edit,
        Permission.create,
        Permission.upload,
        Permission.delete,
      )

      assert(seqAction.map(a => permissionLogic.permitted("FrontPage" , ""          , a)) === "10000".map(_ == '1'))
      assert(seqAction.map(a => permissionLogic.permitted("FrontPage" , actorSomeone, a)) === "10000".map(_ == '1'))
      assert(seqAction.map(a => permissionLogic.permitted("FrontPage" , actorAha00a , a)) === "11111".map(_ == '1'))

      assert(seqAction.map(a => permissionLogic.permitted("Memo" , ""          , a)) === "10000".map(_ == '1'))
      assert(seqAction.map(a => permissionLogic.permitted("Memo" , actorSomeone, a)) === "10000".map(_ == '1'))
      assert(seqAction.map(a => permissionLogic.permitted("Memo" , actorAha00a , a)) === "11111".map(_ == '1'))

      assert(seqAction.map(a => permissionLogic.permitted("other" , ""          , a)) === "00000".map(_ == '1'))
      assert(seqAction.map(a => permissionLogic.permitted("other" , actorSomeone, a)) === "00000".map(_ == '1'))
      assert(seqAction.map(a => permissionLogic.permitted("other" , actorAha00a , a)) === "11111".map(_ == '1'))
    }

    "Protected" in {
      val permissionLogic = new PermissionLogic(Seq(
        Permission(1, decrPriority, "", "aha00a@gmail.com|people@exmaple.com", Permission.admin),
        Permission(1, decrPriority, "", "", Permission.none),
      ))

      val seqAction = Seq(
        Permission.read,
        Permission.edit,
        Permission.create,
        Permission.upload,
        Permission.delete,
      )

      assert(seqAction.map(a => permissionLogic.permitted("FrontPage" , ""          , a)) === "00000".map(_ == '1'))
      assert(seqAction.map(a => permissionLogic.permitted("FrontPage" , actorSomeone, a)) === "00000".map(_ == '1'))
      assert(seqAction.map(a => permissionLogic.permitted("FrontPage" , actorAha00a , a)) === "11111".map(_ == '1'))
    }
  }
}
