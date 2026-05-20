package com.aha00a.tests.unit

import com.aha00a.tests.TestUtil
import logics.wikis.WikiPermission
import models.PageContent
import models.tables.Permission
import models.tables.Permission.ActorType
import models.tables.Permission.TargetType

object WikiPermissionUnit {
  def run(testUtil: TestUtil): Unit = {
    import testUtil.assertEquals

    val legacyOpenContent = PageContent("#!read all\n#!write all\nbody")
    val noRows = WikiPermission.fromRows(Seq.empty)

    assertEquals(noRows.isReadable("Public", legacyOpenContent), false)
    assertEquals(noRows.isReadableByAnonymous("Public", Some(legacyOpenContent)), false)
    assertEquals(noRows.isWritable("Public", legacyOpenContent), false)

    val tableRows = WikiPermission.fromRows(Seq(
      Permission("", TargetType.All, "", ActorType.All, Permission.Action.Read.id),
      Permission("", TargetType.All, "user@example.com", ActorType.Exact, Permission.Action.Create.id),
    ), actor = "user@example.com")

    assertEquals(tableRows.isReadable("Public", PageContent("#!read none\nbody")), true)
    assertEquals(tableRows.isWritable("Public", Some(PageContent("#!write none\nbody"))), true)
    assertEquals(tableRows.isWritable("NewPage", None), true)

    val multiEmailRows = WikiPermission.fromRows(Seq(
      Permission("", TargetType.All, "@example.com", ActorType.Domain, Permission.Action.Read.id),
      Permission("", TargetType.All, "other@example.net", ActorType.Exact, Permission.Action.Create.id),
    ), actors = Seq("user@example.com", "other@example.net"))

    assertEquals(multiEmailRows.isReadable("Public", PageContent("body")), true)
    assertEquals(multiEmailRows.isWritable("NewPage", None), true)
  }
}
