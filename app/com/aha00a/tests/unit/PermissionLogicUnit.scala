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
    val seqAction = Seq(Permission.read, Permission.edit, Permission.create, Permission.upload, Permission.delete)
    def to01(s: Seq[Boolean]): String = s.map(_.to01).mkString

    val public = new PermissionLogic(Seq(Permission("", actorAha00a, Permission.admin), Permission("", "", Permission.edit)))
    assertEquals(to01(seqAction.map(a => public.permitted(targetFrontPage, actorEmpty, a))), "11000")
    assertEquals(to01(seqAction.map(a => public.permitted(targetFrontPage, actorSomeone, a))), "11000")
    assertEquals(to01(seqAction.map(a => public.permitted(targetFrontPage, actorAha00a, a))), "11111")

    val privateP = new PermissionLogic(Seq(Permission("", actorAha00a, Permission.admin), Permission("", "", Permission.none)))
    assertEquals(to01(seqAction.map(a => privateP.permitted(targetFrontPage, actorEmpty, a))), "00000")
    assertEquals(to01(seqAction.map(a => privateP.permitted(targetFrontPage, actorAha00a, a))), "11111")

    val precedence = new PermissionLogic(Seq(
      Permission("Private", Permission.TargetType.StartsWith, "", Permission.ActorType.All, Permission.none),
      Permission("Private/Team", Permission.TargetType.StartsWith, "@example.com", Permission.ActorType.Domain, Permission.read),
      Permission("Private/Team/Plan", Permission.TargetType.Exact, actorAha00a, Permission.ActorType.Exact, Permission.admin),
      Permission("", Permission.TargetType.All, "", Permission.ActorType.All, Permission.read),
    ))
    assertEquals(precedence.matched("Public", actorEmpty).map(_.action), Some(Permission.read))
    assertEquals(precedence.matched("Private/Page", actorSomeone).map(_.action), Some(Permission.none))
    assertEquals(precedence.matched("Private/Team/Page", "user@example.com").map(_.action), Some(Permission.read))
    assertEquals(precedence.matched("Private/Team/Plan", actorAha00a).map(_.action), Some(Permission.admin))

    val noRows = new PermissionLogic(Seq.empty)
    assertEquals(noRows.permitted("AnyPage", actorAha00a, Permission.read), false)
    assertEquals(noRows.matched("AnyPage", actorAha00a), None)
  }
}
