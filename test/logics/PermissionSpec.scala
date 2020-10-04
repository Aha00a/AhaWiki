package logics

import org.scalatest.freespec.AnyFreeSpec

class PermissionSpec extends AnyFreeSpec {

  import models.tables.Permission

  val permissionAnyAnyRead = Permission(1, 1, """""", """""", Permission.read)
  val permissionAnyLoggedIn = Permission(1, 1, """""", """.+""", Permission.read)

  "matches" in {
    import models.tables.Permission


    assert(permissionAnyAnyRead.matches("", "", Permission.read))
    assert(permissionAnyAnyRead.matches("any", "any", Permission.read))

    assert(!permissionAnyLoggedIn.matches("", "", Permission.read))
    assert(permissionAnyAnyRead.matches("any", "any", Permission.read))
  }

  "permitted" in {
    import models.tables.Permission

    assert(permissionAnyAnyRead.permitted(Permission.none))
    assert(permissionAnyAnyRead.permitted(Permission.read))
    assert(!permissionAnyAnyRead.permitted(Permission.edit))
    assert(!permissionAnyAnyRead.permitted(Permission.create))
    assert(!permissionAnyAnyRead.permitted(Permission.upload))
    assert(!permissionAnyAnyRead.permitted(Permission.delete))
  }


}
