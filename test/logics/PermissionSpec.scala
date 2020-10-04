package logics

import org.scalatest.freespec.AnyFreeSpec
import models.tables.Permission

class PermissionSpec extends AnyFreeSpec {

  import models.tables.Permission

  val permissionAnyAnyRead = Permission(1, 1, """""", """""", Permission.read)
  val permissionAnyLoggedInRead = Permission(1, 1, """""", """.+""", Permission.read)
  val permissionAnyAha00aRead = Permission(1, 1, """""", """aha00a@.+""", Permission.read)

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

  "check" in {
    val seq: Seq[Permission] = Seq(permissionAnyAnyRead, permissionAnyLoggedInRead, permissionAnyAha00aRead).sortBy(-_.priority)
    val permissionLogic = new PermissionLogic(seq)
    assert(permissionLogic.check("", "", 0))
  }


}
