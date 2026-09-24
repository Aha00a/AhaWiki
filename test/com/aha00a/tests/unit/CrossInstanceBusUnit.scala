package com.aha00a.tests.unit

import com.aha00a.tests.TestUtil
import logics.CrossInstanceBus
import play.api.libs.json.Json

import java.time.LocalDateTime

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

    // The payload every save path now sends. The browser reads `revision` to drop an event it has
    // already shown and `editorNickname` to name the saver, so both have to be there and be of the
    // type the client expects -- revision a number, not a string.
    // Read back the way the browser does rather than as one string: Json.obj does not promise the
    // order the fields come out in, and the client does not care about it either.
    val built = Json.parse(CrossInstanceBus.pageUpdatedPayload("Home", 7, "aha00a", LocalDateTime.of(2026, 9, 25, 1, 2, 3)))
    assertEquals((built \ "type").as[String], "page.updated")
    assertEquals((built \ "pageName").as[String], "Home")
    assertEquals((built \ "revision").as[Long], 7L)
    assertEquals((built \ "editorNickname").as[String], "aha00a")
    assertEquals((built \ "dateInserted").as[String], "2026-09-25T01:02:03")

    // A page name with a quote or a backslash stays inside the JSON string rather than ending it.
    val quoted = CrossInstanceBus.pageUpdatedPayload("""He said "hi"\n""", 1, "A", LocalDateTime.of(2026, 9, 25, 0, 0, 0))
    assertEquals(CrossInstanceBus.decode(CrossInstanceBus.encode("o", "wiki:1:x", None, quoted)).get.payload, quoted)
  }
}
