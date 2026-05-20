package logics.wikis.macros

import models.tables.Site
import org.scalatest.freespec.AnyFreeSpec

class MacroPercentLinkTitleSpec extends AnyFreeSpec {
  "toHtmlString" - {
    "opens sister wiki links in a new window" in {
      val html = MacroPercentLinkTitle.toHtmlString(
        "0.42, AHA:Some Page, \"\"",
        pageNames = Set.empty,
        sites = Seq(Site(2, "Aha", "AHA", "aha.example")),
      )

      assert(html.contains("""href="https://aha.example/w/Some%20Page""""))
      assert(html.contains("""target="_blank" rel="noopener""""))
      assert(html.contains("AHA:Some Page</a>"))
    }

    "keeps current-site pages in the same window" in {
      val html = MacroPercentLinkTitle.toHtmlString(
        "0.42, AHA:Some Page, \"\"",
        pageNames = Set("AHA:Some Page"),
        sites = Seq(Site(2, "Aha", "AHA", "aha.example")),
      )

      assert(!html.contains("""target="_blank""""))
    }
  }
}
