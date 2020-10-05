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

  val actorSomeone = "someone"
  val actorAha00a = """aha00a@.+"""

  val permissionPrivateAha00aAdmin = Permission(1, decrPriority, targetPrivate + ".*", actorAha00a, Permission.admin)
  val permissionPrivateAnyNone = Permission(1, decrPriority, targetPrivate + ".*", "", Permission.none)

  val permissionAnyAha00aAdmin = Permission(1, decrPriority, "", actorAha00a, Permission.admin)
  val permissionAnyLoggedInEdit = Permission(1, decrPriority, "", ".+", Permission.edit)
  val permissionAnyAnyRead = Permission(1, decrPriority, "", "", Permission.read)

  val seqPermission: Seq[Permission] = Seq(
    permissionPrivateAha00aAdmin,
    permissionPrivateAnyNone,

    permissionAnyAha00aAdmin,
    permissionAnyLoggedInEdit,
    permissionAnyAnyRead,
    
  )

  "permitted" in {
    assert(permissionAnyAnyRead.permitted(Permission.none))
    assert(permissionAnyAnyRead.permitted(Permission.read))
    assert(!permissionAnyAnyRead.permitted(Permission.edit))
    assert(!permissionAnyAnyRead.permitted(Permission.create))
    assert(!permissionAnyAnyRead.permitted(Permission.upload))
    assert(!permissionAnyAnyRead.permitted(Permission.delete))
  }


  "matches" in {
    assert(permissionAnyAnyRead.matches("", ""))
    assert(permissionAnyAnyRead.matches("any", "any"))

    assert(!permissionAnyLoggedInEdit.matches("", ""))
    assert(permissionAnyLoggedInEdit.matches("any", "any"))
  }

  "check" - {

    "anonymous" in {
      val permissionLogic = new PermissionLogic(seqPermission)

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
