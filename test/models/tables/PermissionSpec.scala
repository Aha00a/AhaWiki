package models.tables

import org.scalatest.freespec.AnyFreeSpec

class PermissionSpec extends AnyFreeSpec {
  "" in {
    assert(Permission(1, 1, "", "", Permission.read).matches("", ""))
    assert(!Permission(1, 1, "", "gmail.com", Permission.read).matches("", ""))
  }
}
