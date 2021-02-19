package logics.wikis.macros

import org.scalatest.freespec.AnyFreeSpec

class MacroBrSpec extends AnyFreeSpec {
  "name" in {
    import logics.wikis.RenderingMode
    import models.ContextWikiPage
    import models.ContextSite.Provider
    import models.tables.Site

    implicit val site: Site = Site(-1, "")
    implicit val provider: Provider = Provider.empty
    implicit val wikiContext: ContextWikiPage = new ContextWikiPage(Seq("UnitTest"), RenderingMode.Normal)(null, null, null, null, null)

    val empty = ""
    val dummy = "dummy"
    assert(MacroBr.toHtmlString(empty) === "<br/>")
    assert(MacroBr.toHtmlString(dummy) === "<br/>")
    assert(MacroBr.extractLink(empty) === Seq())
    assert(MacroBr.extractLink(dummy) === Seq())
  }
}
