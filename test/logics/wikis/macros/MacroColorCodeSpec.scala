package logics.wikis.macros

import logics.wikis.RenderingMode
import models.ContextSite.RequestWrapper
import models.ContextWikiPage
import models.tables.Site
import org.scalatest.freespec.AnyFreeSpec

class MacroColorCodeSpec extends AnyFreeSpec {
  "MacroColorCode" in {
    implicit val site: Site = Site.notFound
    implicit val provider: RequestWrapper = RequestWrapper.empty
    implicit val contextWikiPage: ContextWikiPage = new ContextWikiPage(Seq("UnitTest"), RenderingMode.Normal)(null, null, null, null, null)

    val empty = ""
    assert(MacroColorCode.toHtmlString(empty) === "")
    assert(MacroColorCode.toHtmlString("#fff") === """<span><spen class="macroColorCode" style="background-color: #fff"></spen> #fff</span>""")
    assert(MacroColorCode.toHtmlString("#zzz") === """<div class="error">Argument Error - [[ColorCode(#zzz)]]</div>""")

    assert(MacroBr.extractLink(empty) === Seq())
    assert(MacroBr.extractLink("#fff") === Seq())
  }
}
