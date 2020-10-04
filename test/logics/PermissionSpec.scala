package logics

import org.scalatest.freespec.AnyFreeSpec
import models.tables.Permission

class PermissionSpec extends AnyFreeSpec {

  import models.tables.Permission

  val permissionAnyAha00aRead = Permission(1, 3, """""", """aha00a@.+""", Permission.read)
  val permissionAnyLoggedInRead = Permission(1, 2, """""", """.+""", Permission.read)
  val permissionAnyAnyRead = Permission(1, 1, """""", """""", Permission.read)

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
    "anonymous user" in {
      val seq: Seq[Permission] = Seq(
        permissionAnyAnyRead,
        permissionAnyLoggedInRead,
        permissionAnyAha00aRead
      ).sortBy(-_.priority)
      val permissionLogic = new PermissionLogic(seq)
      assert(permissionLogic.check("", "", Permission.read))
      assert(permissionLogic.check("", "a", Permission.read))
      assert(permissionLogic.check("a", "a", Permission.read))
    }
  }


}
