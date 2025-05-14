package logics.wikis.macros

import org.scalatest.freespec.AnyFreeSpec
import provider.EmptyContextWikiPage

class MacroColorCodeSpec extends AnyFreeSpec with EmptyContextWikiPage {
  "MacroColorCode" in {
    val empty = ""
    assert(MacroColorCode.toHtmlString(empty) === "")
    assert(MacroColorCode.toHtmlString("#fff") === """<span class="MacroColorCode"><spen class="preview" style="background-color: #fff"></spen> #fff</span>""")
    assert(MacroColorCode.toHtmlString("#zzz") === """<div class="error">Argument Error - [[ColorCode(#zzz)]]</div>""")

    assert(MacroBr.extractLink(empty) === Seq())
    assert(MacroBr.extractLink("#fff") === Seq())
  }
}
