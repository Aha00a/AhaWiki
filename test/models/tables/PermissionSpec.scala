package models.tables

import org.scalatest.freespec.AnyFreeSpec

class PermissionSpec extends AnyFreeSpec {
  "matches" - {
    "Any Any read" in {
      val permission = Permission(1, "", "", Permission.read)
      assert(permission.matches("", ""))
      assert(permission.matches("", "asdf"))
      assert(permission.matches("asdf", ""))
      assert(permission.matches("asdf", "asdf"))
      assert(permission.actorLevel === 1)
      assert(permission.targetLevel === 1)
    }

    "Any @gmail.com read" in {
      val permission = Permission(1, "", "@gmail.com", Permission.read)
      assert(!permission.matches("", ""))
      assert(permission.matches("", "aha00a@gmail.com"))
      assert(permission.matches("", "aha00b@gmail.com"))
      assert(!permission.matches("", "aha00a@aharise.com"))
      assert(permission.actorLevel === 2)
      assert(permission.targetLevel === 1)
    }

    "Any aha00a@gmail.com admin" in {
      val permission = Permission(1, "", "aha00a@gmail.com", Permission.admin)
      assert(!permission.matches("", ""))
      assert(permission.matches("", "aha00a@gmail.com"))
      assert(!permission.matches("", "aha00b@gmail.com"))
      assert(!permission.matches("", "aha00a@aharise.com"))
      assert(permission.actorLevel === 3)
      assert(permission.targetLevel === 1)
    }

    "Private aha00a@gmail.com admin" in {
      val permission = Permission(1, "Private", "aha00a@gmail.com", Permission.admin)
      assert(!permission.matches("", ""))
      assert(!permission.matches("", "aha00a@gmail.com"))
      assert(!permission.matches("Private", ""))
      assert(permission.matches("Private", "aha00a@gmail.com"))
      assert(!permission.matches("PrivateSomething", "aha00a@gmail.com"))
      assert(permission.actorLevel === 3)
      assert(permission.targetLevel === 3)
    }

    "Private? aha00a@gmail.com admin" in {
      val permission = Permission(1, "Private?", "aha00a@gmail.com", Permission.admin)
      assert(!permission.matches("", ""))
      assert(!permission.matches("", "aha00a@gmail.com"))
      assert(!permission.matches("Private", ""))
      assert(permission.matches("Private", "aha00a@gmail.com"))
      assert(permission.matches("PrivateSomething", "aha00a@gmail.com"))
      assert(permission.actorLevel === 3)
      assert(permission.targetLevel === 2)
    }
  }
}
