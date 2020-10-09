package models.tables

import org.scalatest.freespec.AnyFreeSpec

class PermissionSpec extends AnyFreeSpec {
  "" in {
    assert(Permission(1, 1, "", "", Permission.read).matches("", ""))
  }
}
