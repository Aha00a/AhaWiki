package com.aha00a.tests.unit

import com.aha00a.tests.TestUtil
import logics.CrossInstanceBus

// The channel wire format for cross-instance page.updated. The relay itself needs a running Redis,
// but the encode/decode round trip and the origin/saveSenderId handling are pure and covered here.
object CrossInstanceBusUnit {
  def run(testUtil: TestUtil): Unit = {
    import testUtil.assertEquals

    // Round trip, including a payload that itself contains JSON (so escaping must survive).
    val payload = """{"type":"page.updated","pageName":"Home","revision":5,"editorNickname":"A"}"""
    val decoded = CrossInstanceBus.decode(CrossInstanceBus.encode("origin-1", "wiki:1:Home", Some("s1"), payload))
    assertEquals(decoded.isDefined, true)
    assertEquals(decoded.get.origin, "origin-1")
    assertEquals(decoded.get.roomKey, "wiki:1:Home")
    assertEquals(decoded.get.saveSenderId, Some("s1"))
    assertEquals(decoded.get.payload, payload)

    // No saveSenderId survives as None (not "" or a missing-key failure).
    val noSender = CrossInstanceBus.decode(CrossInstanceBus.encode("o2", "wiki:2:X", None, "p")).get
    assertEquals(noSender.saveSenderId, None)

    // A message from a different instance is foreign; the sender's own is not. (origin comparison
    // is what deliverForeign uses to avoid delivering our own publish twice.)
    val fromOther = CrossInstanceBus.decode(CrossInstanceBus.encode("other", "wiki:1:Home", None, "p")).get
    assertEquals(fromOther.origin != "me", true)

    // Malformed input decodes to None rather than throwing.
    assertEquals(CrossInstanceBus.decode("not json"), None)
    assertEquals(CrossInstanceBus.decode("""{"origin":"o"}"""), None) // missing roomKey/payload
  }
}
