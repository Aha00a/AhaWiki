package com.aha00a.models.tables

import models.tables.Permission
import models.tables.Permission.ActorType
import models.tables.Permission.TargetType
import org.scalatest.freespec.AnyFreeSpec

class PermissionSpec extends AnyFreeSpec {
  "matches" - {
    "matches all targets and actors" in {
      val permission = Permission("", TargetType.All, "", ActorType.All, Permission.Action.Read.id)

      assert(permission.matches("", ""))
      assert(permission.matches("Page", "user@example.com"))
    }

    "requires a non-empty actor for login permissions" in {
      val permission = Permission("", TargetType.All, "", ActorType.Login, Permission.Action.Edit.id)

      assert(!permission.matches("Page", ""))
      assert(permission.matches("Page", "user@example.com"))
    }

    "matches exact and domain actors" in {
      val exact = Permission("", TargetType.All, "user@gmail.com", ActorType.Exact, Permission.Action.Admin.id)
      val domain = Permission("", TargetType.All, "@gmail.com", ActorType.Domain, Permission.Action.Read.id)

      assert(exact.matches("Page", "user@gmail.com"))
      assert(!exact.matches("Page", "other@gmail.com"))
      assert(domain.matches("Page", "other@gmail.com"))
      assert(!domain.matches("Page", "other@example.com"))
    }

    "matches target strategies" in {
      val exact = Permission("Private", TargetType.Exact, "", ActorType.All, Permission.Action.Read.id)
      val startsWith = Permission("Private", TargetType.StartsWith, "", ActorType.All, Permission.Action.Read.id)
      val endsWith = Permission("Private", TargetType.EndsWith, "", ActorType.All, Permission.Action.Read.id)

      assert(exact.matches("Private", ""))
      assert(!exact.matches("Private/Page", ""))
      assert(startsWith.matches("Private/Page", ""))
      assert(!startsWith.matches("Public/Private", ""))
      assert(endsWith.matches("MyPrivate", ""))
      assert(!endsWith.matches("Private/Page", ""))
    }

    "uses regular expressions as full target matches" in {
      val permission = Permission("Private/(Team|Project)/.*", TargetType.RegularExpression, "", ActorType.All, Permission.Action.Read.id)
      val exactFullMatch = Permission("Private", TargetType.RegularExpression, "", ActorType.All, Permission.Action.Read.id)
      val invalidRegex = Permission("[", TargetType.RegularExpression, "", ActorType.All, Permission.Action.Read.id)

      assert(permission.matches("Private/Team/Page", ""))
      assert(permission.matches("Private/Project/Page", ""))
      assert(!permission.matches("Private/Personal/Page", ""))
      assert(!permission.matches("Prefix/Private/Team/Page", ""))
      assert(exactFullMatch.matches("Private", ""))
      assert(!exactFullMatch.matches("MyPrivate", ""))
      assert(!invalidRegex.matches("AnyPage", ""))
    }
  }

  "parseAction" - {
    "accepts action names and known numeric ids" in {
      assert(Permission.parseAction("Read") === Right(Permission.Action.Read.id))
      assert(Permission.parseAction("Create") === Right(Permission.Action.Create.id))
      assert(Permission.parseAction("4") === Right(Permission.Action.Create.id))
      assert(Permission.parseAction("3").isLeft)
    }
  }
}
