package logics.wikis.macros

import org.scalatest.freespec.AnyFreeSpec

class MacroBrSpec extends AnyFreeSpec {
  "name" in {
    import models.WikiContext

    implicit val wikiContext: WikiContext = WikiContext("UnitTest")(null, null, null, null, null)
    val empty = ""
    val dummy = "dummy"
    assert(MacroBr.toHtmlString(empty) === "<br/>")
    assert(MacroBr.toHtmlString(dummy) === "<br/>")
    assert(MacroBr.extractLink(empty) === Seq())
    assert(MacroBr.extractLink(dummy) === Seq())
  }
}
