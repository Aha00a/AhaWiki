package models.tables

import org.scalatest.freespec.AnyFreeSpec

class PermissionSpec extends AnyFreeSpec {
  "" in {
    val permissionAnyAnyRead = Permission(1, 1, "", "", Permission.read)
    val permissionAnyGmailRead = Permission(1, 1, "", "gmail.com", Permission.read)
    val permissionAnyAha00aGmailAdmin = Permission(1, 1, "", "aha00a@gmail.com", Permission.admin)

    assert(permissionAnyAnyRead.matches("", ""))
    assert(permissionAnyAnyRead.matches("", "asdf"))
    assert(permissionAnyAnyRead.matches("asdf", ""))
    assert(permissionAnyAnyRead.matches("asdf", "asdf"))
    assert(permissionAnyAnyRead.actorLevel === 1)

    assert(!permissionAnyGmailRead.matches("", ""))
    assert(!permissionAnyGmailRead.matches("", "aha00a@gmail.com"))
    assert(!permissionAnyGmailRead.matches("", "aha00b@gmail.com"))
    assert(!permissionAnyGmailRead.matches("", "aha00a@aharise.com"))
    assert(permissionAnyGmailRead.actorLevel === 2)

    assert(!permissionAnyAha00aGmailAdmin.matches("", ""))
    assert(permissionAnyAha00aGmailAdmin.matches("", "aha00a@gmail.com"))
    assert(!permissionAnyAha00aGmailAdmin.matches("", "aha00b@gmail.com"))
    assert(!permissionAnyAha00aGmailAdmin.matches("", "aha00a@aharise.com"))
    assert(permissionAnyAha00aGmailAdmin.actorLevel === 3)
  }
}
