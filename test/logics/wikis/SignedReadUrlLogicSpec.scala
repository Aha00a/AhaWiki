package logics.wikis

import org.scalatest.freespec.AnyFreeSpec

class SignedReadUrlLogicSpec extends AnyFreeSpec {
  "verifyReadRequest" - {
    "allows a valid signed read request" in {
      val secret = "test-secret"
      val host = "localhost:9000"
      val name = "PrivatePage"
      val revision = 3
      val action = "view"
      val now = 1000L
      val expires = now + 60
      val signature = SignedReadUrlLogic.signReadRequest(host, name, revision, action, expires, secret)

      assert(SignedReadUrlLogic.verifyReadRequest(
        host = host,
        name = name,
        revision = revision,
        action = action,
        expiresAtEpochSeconds = Some(expires),
        signature = Some(signature),
        secret = secret,
        nowEpochSeconds = now,
      ))
    }

    "rejects expired signed requests" in {
      val secret = "test-secret"
      val host = "localhost:9000"
      val name = "PrivatePage"
      val revision = 0
      val action = "view"
      val now = 1000L
      val expires = now - 1
      val signature = SignedReadUrlLogic.signReadRequest(host, name, revision, action, expires, secret)

      assert(!SignedReadUrlLogic.verifyReadRequest(
        host = host,
        name = name,
        revision = revision,
        action = action,
        expiresAtEpochSeconds = Some(expires),
        signature = Some(signature),
        secret = secret,
        nowEpochSeconds = now,
      ))
    }

    "rejects tampered names" in {
      val secret = "test-secret"
      val host = "localhost:9000"
      val now = 1000L
      val expires = now + 60
      val signature = SignedReadUrlLogic.signReadRequest(host, "PageA", 0, "view", expires, secret)

      assert(!SignedReadUrlLogic.verifyReadRequest(
        host = host,
        name = "PageB",
        revision = 0,
        action = "view",
        expiresAtEpochSeconds = Some(expires),
        signature = Some(signature),
        secret = secret,
        nowEpochSeconds = now,
      ))
    }
  }
}
