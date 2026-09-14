package com.aha00a.logics

import logics.HtmlJson
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import play.api.libs.json.Json

/** JSON that goes into a page has to stay the same JSON to its reader and stay out of the markup's
  * way: nothing in it may end a script element, and nothing may read differently once parsed. */
class HtmlJsonSpec extends AnyFreeSpec with Matchers {

  "a value that would close a script element comes out escaped" in {
    HtmlJson.string("</script><b>x</b>") must not include "</script>"
    HtmlJson.string("</script><b>x</b>") mustBe "\"\\u003c/script\\u003e\\u003cb\\u003ex\\u003c/b\\u003e\""
  }

  "what comes out reads back as the same value" in {
    val value = "Tom & Jerry <3 \u2028 \"quoted\" \\ end"
    Json.parse(HtmlJson.string(value)).as[String] mustBe value
    HtmlJson.string(value) must not include "\u2028"
  }

  "an object is escaped all the way through" in {
    HtmlJson.stringify(Json.obj("name" -> "</script>", "n" -> 1)) mustBe "{\"name\":\"\\u003c/script\\u003e\",\"n\":1}"
  }
}
