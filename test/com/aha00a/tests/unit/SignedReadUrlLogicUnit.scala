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

    // The route's default action is the empty string, so a signature minted for "view" has to
    // cover a URL that never spells the action out.
    assertEquals(SignedReadUrlLogic.normalizeAction(""), Some("view"))
    SignedReadUrlLogic.SignableActions.foreach(action =>
      assertEquals(SignedReadUrlLogic.normalizeAction(action), Some(action))
    )

    // `blame` is served by the same route but is not signable. Signing one action must not
    // hand over another.
    assertEquals(SignedReadUrlLogic.normalizeAction("blame"), None)

    {
      val secret = "test-secret"
      val host = "localhost:9000"
      val now = 1000L
      val expires = now + 60
      val signature = SignedReadUrlLogic.signReadRequest(host, "PageA", 0, "view", expires, secret)

      assertEquals(
        SignedReadUrlLogic.verifyReadRequest(
          host = host,
          name = "PageA",
          revision = 0,
          action = "blame",
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
