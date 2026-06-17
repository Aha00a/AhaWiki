package com.aha00a.tests.unit

import com.aha00a.tests.TestUtil
import models.tables.Permission
import models.tables.Permission.ActorType
import models.tables.Permission.TargetType

object PermissionUnit {
  def run(testUtil: TestUtil): Unit = {
    import testUtil.assertEquals

    {
      val permission = Permission("", TargetType.All, "", ActorType.All, Permission.Action.Read.id)
      assert(permission.matches("", ""))
      assert(permission.matches("", "asdf"))
      assert(permission.matches("asdf", ""))
      assert(permission.matches("asdf", "asdf"))
      assertEquals(permission.actorLevel, 1)
      assertEquals(permission.targetLevel, 1)
    }
    {
      val permission = Permission("", TargetType.All, "", ActorType.Login, Permission.Action.Edit.id)
      assert(!permission.matches("", ""))
      assert(permission.matches("", "aha00a@gmail.com"))
      assert(permission.matches("asdf", "aha00b@example.com"))
      assertEquals(permission.actorLevel, 2)
      assertEquals(permission.targetLevel, 1)
    }
    {
      val permission = Permission("", TargetType.All, "@gmail.com", ActorType.Domain, Permission.Action.Read.id)
      assert(!permission.matches("", ""))
      assert(permission.matches("", "aha00a@gmail.com"))
      assert(permission.matches("", "aha00b@gmail.com"))
      assert(!permission.matches("", "aha00a@aharise.com"))
      assertEquals(permission.actorLevel, 2)
      assertEquals(permission.targetLevel, 1)
    }
    {
      val permission = Permission("", TargetType.All, "aha00a@gmail.com", ActorType.Exact, Permission.Action.Admin.id)
      assert(!permission.matches("", ""))
      assert(permission.matches("", "aha00a@gmail.com"))
      assert(!permission.matches("", "aha00b@gmail.com"))
      assert(!permission.matches("", "aha00a@aharise.com"))
      assertEquals(permission.actorLevel, 3)
      assertEquals(permission.targetLevel, 1)
    }
    {
      val permission = Permission("Private", TargetType.Exact, "aha00a@gmail.com", ActorType.Exact, Permission.Action.Admin.id)
      assert(!permission.matches("", ""))
      assert(!permission.matches("", "aha00a@gmail.com"))
      assert(!permission.matches("Private", ""))
      assert(permission.matches("Private", "aha00a@gmail.com"))
      assert(!permission.matches("PrivateSomething", "aha00a@gmail.com"))
      assertEquals(permission.actorLevel, 3)
      assertEquals(permission.targetLevel, 3)
    }
    {
      val permission = Permission("Private", TargetType.StartsWith, "aha00a@gmail.com", ActorType.Exact, Permission.Action.Admin.id)
      assert(!permission.matches("", ""))
      assert(!permission.matches("", "aha00a@gmail.com"))
      assert(!permission.matches("Private", ""))
      assert(permission.matches("Private", "aha00a@gmail.com"))
      assert(permission.matches("PrivateSomething", "aha00a@gmail.com"))
      assertEquals(permission.actorLevel, 3)
      assertEquals(permission.targetLevel, 2)
    }
    {
      val permission = Permission("Private", TargetType.EndsWith, "aha00a@gmail.com", ActorType.Exact, Permission.Action.Admin.id)
      assert(!permission.matches("PrivateSomething", "aha00a@gmail.com"))
      assert(permission.matches("MyPrivate", "aha00a@gmail.com"))
      assert(permission.matches("Private", "aha00a@gmail.com"))
      assertEquals(permission.actorLevel, 3)
      assertEquals(permission.targetLevel, 2)
    }
    {
      val permission = Permission("Private/(Team|Project)/.*", TargetType.RegularExpression, "aha00a@gmail.com", ActorType.Exact, Permission.Action.Admin.id)
      assert(permission.matches("Private/Team/Page", "aha00a@gmail.com"))
      assert(permission.matches("Private/Project/Page", "aha00a@gmail.com"))
      assert(!permission.matches("Private/Personal/Page", "aha00a@gmail.com"))
      assert(!permission.matches("Public/Team/Page", "aha00a@gmail.com"))
      assert(!permission.matches("Prefix/Private/Team/Page", "aha00a@gmail.com"))
      assert(!permission.matches("Private/Team/Page/Suffix", "other@example.com"))
      assertEquals(permission.actorLevel, 3)
      assertEquals(permission.targetLevel, 2)

      val exactFullMatchRegexPermission = Permission("Private", TargetType.RegularExpression, "", ActorType.All, Permission.Action.Read.id)
      assert(exactFullMatchRegexPermission.matches("Private", ""))
      assert(!exactFullMatchRegexPermission.matches("MyPrivate", ""))
      assert(!exactFullMatchRegexPermission.matches("PrivatePage", ""))

      val invalidRegexPermission = Permission("[", TargetType.RegularExpression, "", ActorType.All, Permission.Action.Read.id)
      assert(!invalidRegexPermission.matches("AnyPage", ""))
      assert(Permission.validate(permission).isRight)
      assert(Permission.validate(invalidRegexPermission).isLeft)
    }
    {
      assertEquals(Permission.parseAction("None"), Right(Permission.Action.None.id))
      assertEquals(Permission.parseAction("Read"), Right(Permission.Action.Read.id))
      assertEquals(Permission.parseAction("Edit"), Right(Permission.Action.Edit.id))
      assertEquals(Permission.parseAction("Create"), Right(Permission.Action.Create.id))
      assertEquals(Permission.parseAction("Upload"), Right(Permission.Action.Upload.id))
      assertEquals(Permission.parseAction("Delete"), Right(Permission.Action.Delete.id))
      assertEquals(Permission.parseAction("Admin"), Right(Permission.Action.Admin.id))
      assertEquals(Permission.parseAction("4"), Right(Permission.Action.Create.id))
      assert(Permission.parseAction("3").isLeft)
      assertEquals(Permission.parseTargetType("StartsWith"), Right(TargetType.StartsWith))
      assertEquals(Permission.parseTargetType("RegularExpression"), Right(TargetType.RegularExpression))
      assertEquals(Permission.parseActorType("Domain"), Right(ActorType.Domain))
      assert(Permission.parseTargetType("Prefix").isLeft)
      assert(Permission.parseActorType("Group").isLeft)
    }
    {
      val createPermission = Permission("", TargetType.All, "", ActorType.All, Permission.Action.Create.id)
      assert(createPermission.permitted(Permission.Action.Read.id))
      assert(createPermission.permitted(Permission.Action.Edit.id))
      assert(createPermission.permitted(Permission.Action.Create.id))
      assert(!createPermission.permitted(Permission.Action.Upload.id))
      assert(!createPermission.permitted(Permission.Action.Delete.id))

      val adminPermission = Permission("", TargetType.All, "", ActorType.All, Permission.Action.Admin.id)
      assert(adminPermission.permitted(Permission.Action.Read.id))
      assert(adminPermission.permitted(Permission.Action.Edit.id))
      assert(adminPermission.permitted(Permission.Action.Create.id))
      assert(adminPermission.permitted(Permission.Action.Upload.id))
      assert(adminPermission.permitted(Permission.Action.Delete.id))
    }
  }
}
