package com.aha00a.tests.unit

import com.aha00a.tests.TestUtil
import logics.wikis.WikiPermission
import models.tables.Permission
import models.tables.Permission.ActorType
import models.tables.Permission.TargetType

object WikiPermissionUnit {
  def run(testUtil: TestUtil): Unit = {
    import testUtil.assertEquals

    // The `#!read all` a page may carry at its top decides nothing: only the rows do.
    val noRows = WikiPermission.fromRows(Seq.empty)

    assertEquals(noRows.isReadable("Public"), false)
    assertEquals(noRows.isReadableByAnonymous("Public"), false)
    assertEquals(noRows.isWritable("Public", pageExists = true), false)

    val tableRows = WikiPermission.fromRows(Seq(
      Permission("", TargetType.All, "", ActorType.All, Permission.Action.Read.id),
      Permission("", TargetType.All, "user@example.com", ActorType.Exact, Permission.Action.Create.id),
    ), actor = "user@example.com")

    assertEquals(tableRows.isReadable("Public"), true)
    assertEquals(tableRows.isWritable("Public", pageExists = true), true)
    assertEquals(tableRows.isWritable("NewPage", pageExists = false), true)

    val multiEmailRows = WikiPermission.fromRows(Seq(
      Permission("", TargetType.All, "@example.com", ActorType.Domain, Permission.Action.Read.id),
      Permission("", TargetType.All, "other@example.net", ActorType.Exact, Permission.Action.Create.id),
    ), actors = Seq("user@example.com", "other@example.net"))

    assertEquals(multiEmailRows.isReadable("Public"), true)
    assertEquals(multiEmailRows.isWritable("NewPage", pageExists = false), true)

    // One answer for every page when no rule names one; none when a rule does.
    assertEquals(tableRows.isReadableRegardlessOfTarget, Some(true))
    assertEquals(noRows.isReadableRegardlessOfTarget, Some(false))
    val pageRule = WikiPermission.fromRows(Seq(
      Permission("Secret", TargetType.Exact, "", ActorType.All, Permission.Action.None.id),
      Permission("", TargetType.All, "", ActorType.All, Permission.Action.Read.id),
    ))
    assertEquals(pageRule.isReadableRegardlessOfTarget, None)
    assertEquals(pageRule.isReadable("Secret"), false)
    assertEquals(pageRule.isReadable("Public"), true)
  }
}
