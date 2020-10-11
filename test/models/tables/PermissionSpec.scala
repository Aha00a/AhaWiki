package models.tables

import org.scalatest.freespec.AnyFreeSpec

class PermissionSpec extends AnyFreeSpec {
  "" in {
    val permissionAnyAnyRead = Permission(1, 1, "", "", Permission.read)
    assert(permissionAnyAnyRead.matches("", ""))
    assert(permissionAnyAnyRead.matches("", "asdf"))
    assert(permissionAnyAnyRead.matches("asdf", ""))
    assert(permissionAnyAnyRead.matches("asdf", "asdf"))
    assert(permissionAnyAnyRead.actorLevel === 1)

    val permissionAnyGmailRead = Permission(1, 1, "", "gmail.com", Permission.read)
    assert(!permissionAnyGmailRead.matches("", ""))
    assert(!permissionAnyGmailRead.matches("", "aha00a@gmail.com"))
    assert(!permissionAnyGmailRead.matches("", "aha00b@gmail.com"))
    assert(!permissionAnyGmailRead.matches("", "aha00a@aharise.com"))
    assert(permissionAnyGmailRead.actorLevel === 2)
  }
}
