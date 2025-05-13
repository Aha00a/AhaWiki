package logics.wikis.macros

import models.ContextWikiPage
import org.scalatest.freespec.AnyFreeSpec
import provider.EmptyContextWikiPage

class MacroBrSpec extends AnyFreeSpec with EmptyContextWikiPage{
  "name" in {
    implicit val contextWikiPage: ContextWikiPage = createContextWikiPage();

    val empty = ""
    val dummy = "dummy"
    assert(MacroBr.toHtmlString(empty) === "<br/>")
    assert(MacroBr.toHtmlString(dummy) === "<br/>")
    assert(MacroBr.extractLink(empty) === Seq())
    assert(MacroBr.extractLink(dummy) === Seq())
  }
}
