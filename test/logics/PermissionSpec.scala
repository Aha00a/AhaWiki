package logics

import org.scalatest.freespec.AnyFreeSpec

class PermissionSpec extends AnyFreeSpec {

  import models.tables.Permission

  val permissionAnyAnyRead = Permission(1, 1, """""", """""", Permission.read)
  val permissionAnyLoggedInRead = Permission(1, 1, """""", """.+""", Permission.read)
  val permissionAnyAha00aRead = Permission(1, 1, """""", """aha00a@.+""", Permission.read)

  "permitted" in {
    import models.tables.Permission

    assert(permissionAnyAnyRead.permitted(Permission.none))
    assert(permissionAnyAnyRead.permitted(Permission.read))
    assert(!permissionAnyAnyRead.permitted(Permission.edit))
    assert(!permissionAnyAnyRead.permitted(Permission.create))
    assert(!permissionAnyAnyRead.permitted(Permission.upload))
    assert(!permissionAnyAnyRead.permitted(Permission.delete))
  }


  "matches" in {
    import models.tables.Permission


    assert(permissionAnyAnyRead.matches("", "", Permission.read))
    assert(permissionAnyAnyRead.matches("any", "any", Permission.read))

    assert(!permissionAnyLoggedInRead.matches("", "", Permission.read))
    assert(permissionAnyLoggedInRead.matches("any", "any", Permission.read))
  }


}
