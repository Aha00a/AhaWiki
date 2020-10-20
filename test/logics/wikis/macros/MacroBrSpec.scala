package logics.wikis.macros

import org.scalatest.freespec.AnyFreeSpec

class MacroBrSpec extends AnyFreeSpec {
  "name" in {
    import logics.AhaWikiInjects
    import logics.wikis.RenderingMode
    import models.WikiContext
    import models.WikiContext.Provider

    implicit val ahaWikiInjects: AhaWikiInjects = AhaWikiInjects()(null, null, null, null, null)
    implicit val wikiContext: WikiContext = new WikiContext(Seq("UnitTest"), RenderingMode.Normal)(ahaWikiInjects, Provider.empty)
    val empty = ""
    val dummy = "dummy"
    assert(MacroBr.toHtmlString(empty) === "<br/>")
    assert(MacroBr.toHtmlString(dummy) === "<br/>")
    assert(MacroBr.extractLink(empty) === Seq())
    assert(MacroBr.extractLink(dummy) === Seq())
  }
}
