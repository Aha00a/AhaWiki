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

    // MacroSimilarPages writes the page name in unquoted; this name broke a list on aha00a.com.
    "takes a page name with commas whole" in {
      val html = MacroPercentLinkTitle.toHtmlString(
        "0.34, 사랑따윈 필요없어, 여름, \"\"",
        pageNames = Set("사랑따윈 필요없어, 여름"),
        sites = Seq.empty,
      )

      assert(html.contains("사랑따윈 필요없어, 여름</a>"))
      assert(html.contains("34.00%"))
    }

    "keeps a title with commas apart from the link" in {
      val html = MacroPercentLinkTitle.toHtmlString(
        "0.5, A, B, \"x, y\"",
        pageNames = Set("A, B"),
        sites = Seq.empty,
      )

      assert(html.contains("A, B</a>"))
      assert(html.contains("<span>x, y</span>"))
    }
  }
}
