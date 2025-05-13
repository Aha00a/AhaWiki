package logics.wikis.macros

import models.ContextWikiPage
import org.scalatest.freespec.AnyFreeSpec
import org.scalatestplus.play.guice.GuiceOneAppPerTest
import provider.RealContextWikiPage

// TODO: fix to use EmptyContextWikiPage
//class MacroColorCodeSpec extends AnyFreeSpec with EmptyContextWikiPage {
class MacroColorCodeSpec extends AnyFreeSpec with GuiceOneAppPerTest with RealContextWikiPage {
  "MacroColorCode" in {

    implicit val contextWikiPage: ContextWikiPage = createContextWikiPage();

    val empty = ""
    assert(MacroColorCode.toHtmlString(empty) === "")
    assert(MacroColorCode.toHtmlString("#fff") === """<span class="MacroColorCode"><spen class="preview" style="background-color: #fff"></spen> #fff</span>""")
    assert(MacroColorCode.toHtmlString("#zzz") === """<div class="error">Argument Error - [[ColorCode(#zzz)]]</div>""")

    assert(MacroBr.extractLink(empty) === Seq())
    assert(MacroBr.extractLink("#fff") === Seq())
  }
}
