package com.aha00a.tests.unit

import com.aha00a.tests.TestUtil
import logics.wikis.SignedReadUrlLogic

object SignedReadUrlLogicUnit {
  def run(testUtil: TestUtil): Unit = {
    import testUtil.assertEquals

    {
      val secret = "test-secret"
      val host = "localhost:9000"
      val name = "PrivatePage"
      val revision = 3
      val action = "view"
      val now = 1000L
      val expires = now + 60
      val signature = SignedReadUrlLogic.signReadRequest(host, name, revision, action, expires, secret)

      assertEquals(
        SignedReadUrlLogic.verifyReadRequest(
          host = host,
          name = name,
          revision = revision,
          action = action,
          expiresAtEpochSeconds = Some(expires),
          signature = Some(signature),
          secret = secret,
          nowEpochSeconds = now,
        ),
        true,
      )
    }

    {
      val secret = "test-secret"
      val host = "localhost:9000"
      val name = "PrivatePage"
      val revision = 0
      val action = "view"
      val now = 1000L
      val expires = now - 1
      val signature = SignedReadUrlLogic.signReadRequest(host, name, revision, action, expires, secret)

      assertEquals(
        SignedReadUrlLogic.verifyReadRequest(
          host = host,
          name = name,
          revision = revision,
          action = action,
          expiresAtEpochSeconds = Some(expires),
          signature = Some(signature),
          secret = secret,
          nowEpochSeconds = now,
        ),
        false,
      )
    }

    {
      val secret = "test-secret"
      val host = "localhost:9000"
      val now = 1000L
      val expires = now + 60
      val signature = SignedReadUrlLogic.signReadRequest(host, "PageA", 0, "view", expires, secret)

      assertEquals(
        SignedReadUrlLogic.verifyReadRequest(
          host = host,
          name = "PageB",
          revision = 0,
          action = "view",
          expiresAtEpochSeconds = Some(expires),
          signature = Some(signature),
          secret = secret,
          nowEpochSeconds = now,
        ),
        false,
      )
    }
  }
}
