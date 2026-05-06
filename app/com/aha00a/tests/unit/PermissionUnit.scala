package com.aha00a.tests.unit

import com.aha00a.tests.TestUtil
import models.tables.Permission

object PermissionUnit {
  def run(testUtil: TestUtil): Unit = {
    import testUtil.assertEquals

    {
      val permission = Permission("", "", Permission.read)
      assert(permission.matches("", ""))
      assert(permission.matches("", "asdf"))
      assert(permission.matches("asdf", ""))
      assert(permission.matches("asdf", "asdf"))
      assertEquals(permission.actorLevel, 1)
      assertEquals(permission.targetLevel, 1)
    }
    {
      val permission = Permission("", "@gmail.com", Permission.read)
      assert(!permission.matches("", ""))
      assert(permission.matches("", "aha00a@gmail.com"))
      assert(permission.matches("", "aha00b@gmail.com"))
      assert(!permission.matches("", "aha00a@aharise.com"))
      assertEquals(permission.actorLevel, 2)
      assertEquals(permission.targetLevel, 1)
    }
    {
      val permission = Permission("", "aha00a@gmail.com", Permission.admin)
      assert(!permission.matches("", ""))
      assert(permission.matches("", "aha00a@gmail.com"))
      assert(!permission.matches("", "aha00b@gmail.com"))
      assert(!permission.matches("", "aha00a@aharise.com"))
      assertEquals(permission.actorLevel, 3)
      assertEquals(permission.targetLevel, 1)
    }
    {
      val permission = Permission("Private", "aha00a@gmail.com", Permission.admin)
      assert(!permission.matches("", ""))
      assert(!permission.matches("", "aha00a@gmail.com"))
      assert(!permission.matches("Private", ""))
      assert(permission.matches("Private", "aha00a@gmail.com"))
      assert(!permission.matches("PrivateSomething", "aha00a@gmail.com"))
      assertEquals(permission.actorLevel, 3)
      assertEquals(permission.targetLevel, 3)
    }
    {
      val permission = Permission("Private?", "aha00a@gmail.com", Permission.admin)
      assert(!permission.matches("", ""))
      assert(!permission.matches("", "aha00a@gmail.com"))
      assert(!permission.matches("Private", ""))
      assert(permission.matches("Private", "aha00a@gmail.com"))
      assert(permission.matches("PrivateSomething", "aha00a@gmail.com"))
      assertEquals(permission.actorLevel, 3)
      assertEquals(permission.targetLevel, 2)
    }
  }
}
