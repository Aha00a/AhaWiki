package logics.wikis.macros

import logics.wikis.ExtractConvertInjectMacro
import models.tables.Site
import org.scalatest.freespec.AnyFreeSpec

class MacroAhaWikiSiteListSpec extends AnyFreeSpec {
  "publicSites" - {
    "keeps only positive listed sites with domains and sorts high order first" in {
      val sites = Seq(
        Site(1, "HiddenNull", "HN", "hidden-null.example", None),
        Site(2, "HiddenZero", "HZ", "hidden-zero.example", Some(BigDecimal("0.00"))),
        Site(3, "VisibleLow", "VL", "low.example", Some(BigDecimal("10.00"))),
        Site(4, "VisibleHigh", "VH", "high.example", Some(BigDecimal("20.00"))),
        Site(5, "NoDomain", "ND", "", Some(BigDecimal("30.00"))),
        Site(6, "VisibleTie", "VT", "tie.example", Some(BigDecimal("10.00"))),
      )

      assert(MacroAhaWikiSiteList.publicSites(sites).map(_.seq) === Seq(4, 3, 6))
    }
  }

  "render" - {
    "renders a site list with favicons" in {
      val html = MacroAhaWikiSiteList.render(Seq(Site(1, "AhaWiki", "AHA", "ahawiki.net", Some(BigDecimal("100.00")))))

      assert(html.startsWith("""<ul class="MacroAhaWikiSiteList">"""))
      assert(html.contains("""href="https://ahawiki.net""""))
      assert(html.contains("""src="https://ahawiki.net/favicon.ico""""))
      assert(html.contains("""onerror="this.onerror=null;this.src='/public/favicon.png';""""))
      assert(html.contains(">AhaWiki</a>"))
    }

    "escapes domains in attributes and names in text" in {
      val html = MacroAhaWikiSiteList.render(Seq(Site(1, """Unsafe <Site>""", "UN", """evil.example" onclick="alert(1)""", Some(BigDecimal("100.00")))))

      assert(html.contains("""href="https://evil.example&quot; onclick=&quot;alert(1)""""))
      assert(html.contains("""src="https://evil.example&quot; onclick=&quot;alert(1)/favicon.ico""""))
      assert(html.contains("""Unsafe &lt;Site&gt;</a>"""))
    }
  }

  "registration" - {
    "is registered as a block wiki macro" in {
      assert(ExtractConvertInjectMacro.macroNames.contains("AhaWikiSiteList"))
      assert(MacroAhaWikiSiteList.isBlock)
    }
  }
}
