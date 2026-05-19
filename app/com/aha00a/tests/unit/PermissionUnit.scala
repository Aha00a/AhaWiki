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
    {
      val permission = Permission("^Private/(Team|Project)/.*$", TargetType.RegularExpression, "aha00a@gmail.com", ActorType.Exact, Permission.admin)
      assert(permission.matches("Private/Team/Page", "aha00a@gmail.com"))
      assert(permission.matches("Private/Project/Page", "aha00a@gmail.com"))
      assert(!permission.matches("Private/Personal/Page", "aha00a@gmail.com"))
      assert(!permission.matches("Public/Team/Page", "aha00a@gmail.com"))
      assertEquals(permission.actorLevel, 3)
      assertEquals(permission.targetLevel, 2)

      val invalidRegexPermission = Permission("[", TargetType.RegularExpression, "", ActorType.All, Permission.read)
      assert(!invalidRegexPermission.matches("AnyPage", ""))
      assert(Permission.validate(permission).isRight)
      assert(Permission.validate(invalidRegexPermission).isLeft)
    }
    {
      assertEquals(Permission.Action.none.id, Permission.none)
      assertEquals(Permission.Action.read.id, Permission.read)
      assertEquals(Permission.Action.edit.id, Permission.edit)
      assertEquals(Permission.Action.create.id, Permission.create)
      assertEquals(Permission.Action.upload.id, Permission.upload)
      assertEquals(Permission.Action.delete.id, Permission.delete)
      assertEquals(Permission.Action.admin.id, Permission.admin)
      assertEquals(Permission.parseAction("read"), Right(Permission.read))
      assertEquals(Permission.parseAction("4"), Right(Permission.create))
      assert(Permission.parseAction("3").isLeft)
      assertEquals(Permission.parseTargetType("StartsWith"), Right(TargetType.StartsWith))
      assertEquals(Permission.parseTargetType("RegularExpression"), Right(TargetType.RegularExpression))
      assertEquals(Permission.parseActorType("Domain"), Right(ActorType.Domain))
      assert(Permission.parseTargetType("Prefix").isLeft)
      assert(Permission.parseActorType("Group").isLeft)
    }
    {
      val createPermission = Permission("", TargetType.All, "", ActorType.All, Permission.create)
      assert(createPermission.permitted(Permission.read))
      assert(createPermission.permitted(Permission.edit))
      assert(createPermission.permitted(Permission.create))
      assert(!createPermission.permitted(Permission.upload))
      assert(!createPermission.permitted(Permission.delete))

      val adminPermission = Permission("", TargetType.All, "", ActorType.All, Permission.admin)
      assert(adminPermission.permitted(Permission.read))
      assert(adminPermission.permitted(Permission.edit))
      assert(adminPermission.permitted(Permission.create))
      assert(adminPermission.permitted(Permission.upload))
      assert(adminPermission.permitted(Permission.delete))
    }
  }
}
