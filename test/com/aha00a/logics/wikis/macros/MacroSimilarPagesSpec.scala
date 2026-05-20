package logics.wikis.macros

import models.tables.CalculatedCosineSimilarity
import models.tables.Site
import org.scalatest.freespec.AnyFreeSpec

class MacroSimilarPagesSpec extends AnyFreeSpec {
  "formatSimilarPagesMarkup" - {
    "separates same wiki and sister wiki groups" in {
      val markup = MacroSimilarPages.formatSimilarPagesMarkup(
        sameSiteSimilarities = Seq(CalculatedCosineSimilarity(1, "Foo", 1, "Bar", 0.8)),
        crossSiteSimilarities = Seq(CalculatedCosineSimilarity(1, "Foo", 2, "Baz", 0.7)),
        siteBySeq = Map(
          1L -> Site(1, "Current", "CUR", "current.example"),
          2L -> Site(2, "Sister", "SIS", "sister.example"),
        ),
        currentSiteSeq = 1,
        highScoredTerms = Map(
          (1L, "Bar") -> "alpha(2:3)",
          (2L, "Baz") -> "beta(4:5)",
        ),
      )

      assert(markup === """ * Same Wiki
  1. [[PercentLinkTitle(0.8, Bar, "")]] [[Trivial(alpha(2:3))]]
 * Sister Wikis
  1. [[PercentLinkTitle(0.7, SIS:Baz, "")]] [[Trivial(beta(4:5))]]""")
    }

    "omits group bullets when only sister wiki pages exist" in {
      val markup = MacroSimilarPages.formatSimilarPagesMarkup(
        sameSiteSimilarities = Seq.empty,
        crossSiteSimilarities = Seq(CalculatedCosineSimilarity(1, "Foo", 2, "Baz", 0.7)),
        siteBySeq = Map(2L -> Site(2, "Sister", "SIS", "sister.example")),
        currentSiteSeq = 1,
        highScoredTerms = Map.empty,
      )

      assert(!markup.contains("Same Wiki"))
      assert(!markup.contains("Sister Wikis"))
      assert(markup === """ 1. [[PercentLinkTitle(0.7, SIS:Baz, "")]] [[Trivial()]]""")
    }

    "omits group bullets when only same wiki pages exist" in {
      val markup = MacroSimilarPages.formatSimilarPagesMarkup(
        sameSiteSimilarities = Seq(CalculatedCosineSimilarity(1, "Foo", 1, "Bar", 0.8)),
        crossSiteSimilarities = Seq.empty,
        siteBySeq = Map(1L -> Site(1, "Current", "CUR", "current.example")),
        currentSiteSeq = 1,
        highScoredTerms = Map.empty,
      )

      assert(!markup.contains("Same Wiki"))
      assert(!markup.contains("Sister Wikis"))
      assert(markup === """ 1. [[PercentLinkTitle(0.8, Bar, "")]] [[Trivial()]]""")
    }
  }
}
