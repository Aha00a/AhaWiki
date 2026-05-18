package com.aha00a.tests.unit

import com.aha00a.tests.TestUtil
import models.tables.Permission
import models.tables.Permission.ActorType
import models.tables.Permission.TargetType

object PermissionUnit {
  def run(testUtil: TestUtil): Unit = {
    import testUtil.assertEquals

    {
      val permission = Permission("", TargetType.All, "", ActorType.All, Permission.read)
      assert(permission.matches("", ""))
      assert(permission.matches("", "asdf"))
      assert(permission.matches("asdf", ""))
      assert(permission.matches("asdf", "asdf"))
      assertEquals(permission.actorLevel, 1)
      assertEquals(permission.targetLevel, 1)
    }
    {
      val permission = Permission("", TargetType.All, "", ActorType.Login, Permission.edit)
      assert(!permission.matches("", ""))
      assert(permission.matches("", "aha00a@gmail.com"))
      assert(permission.matches("asdf", "aha00b@example.com"))
      assertEquals(permission.actorLevel, 2)
      assertEquals(permission.targetLevel, 1)
    }
    {
      val permission = Permission("", TargetType.All, "@gmail.com", ActorType.Domain, Permission.read)
      assert(!permission.matches("", ""))
      assert(permission.matches("", "aha00a@gmail.com"))
      assert(permission.matches("", "aha00b@gmail.com"))
      assert(!permission.matches("", "aha00a@aharise.com"))
      assertEquals(permission.actorLevel, 2)
      assertEquals(permission.targetLevel, 1)
    }
    {
      val permission = Permission("", TargetType.All, "aha00a@gmail.com", ActorType.Exact, Permission.admin)
      assert(!permission.matches("", ""))
      assert(permission.matches("", "aha00a@gmail.com"))
      assert(!permission.matches("", "aha00b@gmail.com"))
      assert(!permission.matches("", "aha00a@aharise.com"))
      assertEquals(permission.actorLevel, 3)
      assertEquals(permission.targetLevel, 1)
    }
    {
      val permission = Permission("Private", TargetType.Exact, "aha00a@gmail.com", ActorType.Exact, Permission.admin)
      assert(!permission.matches("", ""))
      assert(!permission.matches("", "aha00a@gmail.com"))
      assert(!permission.matches("Private", ""))
      assert(permission.matches("Private", "aha00a@gmail.com"))
      assert(!permission.matches("PrivateSomething", "aha00a@gmail.com"))
      assertEquals(permission.actorLevel, 3)
      assertEquals(permission.targetLevel, 3)
    }
    {
      val permission = Permission("Private", TargetType.StartsWith, "aha00a@gmail.com", ActorType.Exact, Permission.admin)
      assert(!permission.matches("", ""))
      assert(!permission.matches("", "aha00a@gmail.com"))
      assert(!permission.matches("Private", ""))
      assert(permission.matches("Private", "aha00a@gmail.com"))
      assert(permission.matches("PrivateSomething", "aha00a@gmail.com"))
      assertEquals(permission.actorLevel, 3)
      assertEquals(permission.targetLevel, 2)
    }
    {
      val permission = Permission("Private", TargetType.EndsWith, "aha00a@gmail.com", ActorType.Exact, Permission.admin)
      assert(!permission.matches("PrivateSomething", "aha00a@gmail.com"))
      assert(permission.matches("MyPrivate", "aha00a@gmail.com"))
      assert(permission.matches("Private", "aha00a@gmail.com"))
      assertEquals(permission.actorLevel, 3)
      assertEquals(permission.targetLevel, 2)
    }
  }
}
