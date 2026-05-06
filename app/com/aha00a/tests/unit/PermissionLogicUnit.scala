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
  }
}
