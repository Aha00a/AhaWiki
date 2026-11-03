package com.aha00a.logics.wikis.interpreters

import com.aha00a.tests.provider.EmptyContextWikiPage
import logics.wikis.RenderingMode
import logics.wikis.interpreters.ahaMark.AhaMarkLink
import models.ContextWikiPage
import org.scalatest.freespec.AnyFreeSpec

/** Where a page link's page name ends, and the address that name is drawn with. */
class AhaMarkLinkSpec extends AnyFreeSpec with EmptyContextWikiPage {
  private implicit val context: ContextWikiPage = new ContextWikiPage(Seq("Elsewhere"), RenderingMode.Normal)

  "the page name" - {
    "is the whole link when nothing follows it" in {
      assert(AhaMarkLink.splitSuffix("FrontPage") === ("FrontPage", ""))
    }

    "stops at an anchor" in {
      assert(AhaMarkLink.splitSuffix("Page#section") === ("Page", "#section"))
    }

    "stops at a query" in {
      assert(AhaMarkLink.splitSuffix("Page?revision=3") === ("Page", "?revision=3"))
      assert(AhaMarkLink.splitSuffix("Page?revision=3#section") === ("Page", "?revision=3#section"))
    }

    "keeps a ? that does not start a query" in {
      assert(AhaMarkLink.splitSuffix("What If...?: Season One") === ("What If...?: Season One", ""))
      assert(AhaMarkLink.splitSuffix("What If...?#Cast") === ("What If...?", "#Cast"))
    }
  }

  "a ? in the name" - {
    val pages = Set("What If...?: Season One")

    "is escaped in the address, so the browser does not read the rest as a query" in {
      val html = AhaMarkLink("What If...?: Season One").toHtmlString(pages)

      assert(html.contains("""href="/w/What If...%3F: Season One""""))
    }

    "finds the page, so the link is not drawn as missing" in {
      val html = AhaMarkLink("What If...?: Season One").toHtmlString(pages)

      assert(!html.contains("missing"))
    }
  }

  "a query after a name is left as it was" in {
    val html = AhaMarkLink("Page?revision=3").toHtmlString(Set("Page"))

    assert(html.contains("""href="/w/Page?revision=3""""))
    assert(!html.contains("missing"))
  }
}
