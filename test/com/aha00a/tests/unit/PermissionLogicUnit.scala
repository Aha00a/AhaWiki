package com.aha00a.tests.unit

import com.aha00a.commons.Implicits._
import com.aha00a.tests.TestUtil
import logics.PermissionLogic
import models.tables.Permission

object PermissionLogicUnit {
  def run(testUtil: TestUtil): Unit = {
    import testUtil.assertEquals

    val targetFrontPage = "FrontPage"
    val actorEmpty = ""
    val actorSomeone = "aha00a+someone@gmail.com"
    val actorAha00a = "aha00a@gmail.com"
    val seqAction = Seq(Permission.Action.Read.id, Permission.Action.Edit.id, Permission.Action.Create.id, Permission.Action.Upload.id, Permission.Action.Delete.id)
    def to01(s: Seq[Boolean]): String = s.map(_.to01).mkString

    val public = new PermissionLogic(Seq(
      Permission("", Permission.TargetType.All, actorAha00a, Permission.ActorType.Exact, Permission.Action.Admin.id),
      Permission("", Permission.TargetType.All, "", Permission.ActorType.All, Permission.Action.Edit.id),
    ))
    assertEquals(to01(seqAction.map(a => public.permitted(targetFrontPage, actorEmpty, a))), "11000")
    assertEquals(to01(seqAction.map(a => public.permitted(targetFrontPage, actorSomeone, a))), "11000")
    assertEquals(to01(seqAction.map(a => public.permitted(targetFrontPage, actorAha00a, a))), "11111")

    val privateP = new PermissionLogic(Seq(
      Permission("", Permission.TargetType.All, actorAha00a, Permission.ActorType.Exact, Permission.Action.Admin.id),
      Permission("", Permission.TargetType.All, "", Permission.ActorType.All, Permission.Action.None.id),
    ))
    assertEquals(to01(seqAction.map(a => privateP.permitted(targetFrontPage, actorEmpty, a))), "00000")
    assertEquals(to01(seqAction.map(a => privateP.permitted(targetFrontPage, actorAha00a, a))), "11111")

    val precedence = new PermissionLogic(Seq(
      Permission("Private", Permission.TargetType.StartsWith, "", Permission.ActorType.All, Permission.Action.None.id),
      Permission("Private/Team", Permission.TargetType.StartsWith, "@example.com", Permission.ActorType.Domain, Permission.Action.Read.id),
      Permission("Private/Team/Plan", Permission.TargetType.Exact, actorAha00a, Permission.ActorType.Exact, Permission.Action.Admin.id),
      Permission("", Permission.TargetType.All, "", Permission.ActorType.All, Permission.Action.Read.id),
    ))
    assertEquals(precedence.matched("Public", actorEmpty).map(_.action), Some(Permission.Action.Read.id))
    assertEquals(precedence.matched("Private/Page", actorSomeone).map(_.action), Some(Permission.Action.None.id))
    assertEquals(precedence.matched("Private/Team/Page", "user@example.com").map(_.action), Some(Permission.Action.Read.id))
    assertEquals(precedence.matched("Private/Team/Plan", actorAha00a).map(_.action), Some(Permission.Action.Admin.id))

    val regularExpression = new PermissionLogic(Seq(
      Permission("Private/(Team|Project)/.*", Permission.TargetType.RegularExpression, "", Permission.ActorType.All, Permission.Action.Read.id),
      Permission("Private/Team/Plan", Permission.TargetType.Exact, "", Permission.ActorType.All, Permission.Action.None.id),
      Permission("", Permission.TargetType.All, "", Permission.ActorType.All, Permission.Action.None.id),
    ))
    assertEquals(regularExpression.matched("Private/Team/Page", actorEmpty).map(_.action), Some(Permission.Action.Read.id))
    assertEquals(regularExpression.matched("Private/Project/Page", actorEmpty).map(_.action), Some(Permission.Action.Read.id))
    assertEquals(regularExpression.matched("Private/Personal/Page", actorEmpty).map(_.action), Some(Permission.Action.None.id))
    assertEquals(regularExpression.matched("Prefix/Private/Team/Page", actorEmpty).map(_.action), Some(Permission.Action.None.id))
    assertEquals(regularExpression.matched("Private/Team/Plan", actorEmpty).map(_.action), Some(Permission.Action.None.id))

    val noRows = new PermissionLogic(Seq.empty)
    assertEquals(noRows.permitted("AnyPage", actorAha00a, Permission.Action.Read.id), false)
    assertEquals(noRows.matched("AnyPage", actorAha00a), None)

    // Equal specificity is broken by length, target first: two StartsWith+All rows both
    // score 2+1, and the longer target is the one that speaks for the narrower subtree.
    val targetLength = new PermissionLogic(Seq(
      Permission("Private", Permission.TargetType.StartsWith, "", Permission.ActorType.All, Permission.Action.None.id),
      Permission("Private/Team", Permission.TargetType.StartsWith, "", Permission.ActorType.All, Permission.Action.Read.id),
    ))
    assertEquals(targetLength.matched("Private/Team/Page", actorEmpty).map(_.action), Some(Permission.Action.Read.id))
    assertEquals(targetLength.matched("Private/Page", actorEmpty).map(_.action), Some(Permission.Action.None.id))

    // Same for actors: two Domain rows both score 2, and the longer suffix is the narrower
    // audience, so it wins for the addresses it matches.
    val actorLength = new PermissionLogic(Seq(
      Permission("", Permission.TargetType.All, "@example.com", Permission.ActorType.Domain, Permission.Action.Read.id),
      Permission("", Permission.TargetType.All, "@team.example.com", Permission.ActorType.Domain, Permission.Action.Edit.id),
    ))
    assertEquals(actorLength.matched("AnyPage", "a@team.example.com").map(_.action), Some(Permission.Action.Edit.id))
    assertEquals(actorLength.matched("AnyPage", "a@example.com").map(_.action), Some(Permission.Action.Read.id))
  }
}
