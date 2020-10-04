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

  val targetPrivate = "Privete"

  val actorSomeone = "sdfkj"
  val actorAha00a = """aha00a@.+"""

  val permissionAnyAha00aRead = Permission(1, decrPriority, targetPrivate + """.*""", actorAha00a, Permission.read)
  val permissionAnyLoggedInRead = Permission(1, decrPriority, """""", """.+""", Permission.read)
  val permissionAnyAnyRead = Permission(1, decrPriority, """""", """""", Permission.read)
  val seqPermission: Seq[Permission] = Seq(
    permissionAnyAnyRead,
    permissionAnyLoggedInRead,
    permissionAnyAha00aRead
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

    assert(!permissionAnyLoggedInRead.matches("", ""))
    assert(permissionAnyLoggedInRead.matches("any", "any"))
  }

  "check" - {

    "anonymous" in {
      val permissionLogic = new PermissionLogic(seqPermission)

      assert(permissionLogic.check("", "", Permission.read))
      assert(permissionLogic.check("", "", Permission.read))
      assert(permissionLogic.check("a", "", Permission.read))
      assert(permissionLogic.check("a", actorSomeone, Permission.read))
      assert(permissionLogic.check(targetPrivate, "", Permission.read))
      assert(permissionLogic.check(targetPrivate, actorSomeone, Permission.read))
      assert(permissionLogic.check(targetPrivate, actorAha00a, Permission.read))
    }
  }


}
