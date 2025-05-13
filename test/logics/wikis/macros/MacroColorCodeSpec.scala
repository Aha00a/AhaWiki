package logics.wikis.macros

import logics.wikis.RenderingMode
import models.RequestWrapper
import models.ContextWikiPage
import models.tables.Site
import org.scalatest.freespec.AnyFreeSpec

class MacroColorCodeSpec extends AnyFreeSpec {
  "MacroColorCode" in {
    implicit val contextWikiPage: ContextWikiPage = new ContextWikiPage(Seq("UnitTest"), RenderingMode.Normal)(null, null, null, null, RequestWrapper.empty, Site.notFound)

    val empty = ""
    assert(MacroColorCode.toHtmlString(empty) === "")
    assert(MacroColorCode.toHtmlString("#fff") === """<span><spen class="macroColorCode" style="background-color: #fff"></spen> #fff</span>""")
    assert(MacroColorCode.toHtmlString("#zzz") === """<div class="error">Argument Error - [[ColorCode(#zzz)]]</div>""")

    assert(MacroBr.extractLink(empty) === Seq())
    assert(MacroBr.extractLink("#fff") === Seq())
  }
}
