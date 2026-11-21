package logics.wikis.macros

import models.tables.CalculatedCosineSimilarity
import models.tables.Site
import org.scalatest.freespec.AnyFreeSpec

/**
 * The list's markup: its groups, its order, and what each line is handed to be drawn from.
 *
 * A line is drawn by the function the caller passes. The macro passes one that answers with a
 * placeholder, so that no page name is ever in the markup (SisterWikiNamesSpec holds that, through
 * the renderer); here it writes the three values out, to show what each line was given.
 */
class MacroSimilarPagesSpec extends AnyFreeSpec {
  private val written: MacroSimilarPages.Line => String =
    line => s"<${line.link} ${line.similarity} ${line.terms}>"

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
      )(written)

      assert(markup === """ * Same Wiki
  1. <Bar 0.8 alpha(2:3)>
 * Sister Wikis
  1. <SIS:Baz 0.7 beta(4:5)>""")
    }

    "omits group bullets when only sister wiki pages exist" in {
      val markup = MacroSimilarPages.formatSimilarPagesMarkup(
        sameSiteSimilarities = Seq.empty,
        crossSiteSimilarities = Seq(CalculatedCosineSimilarity(1, "Foo", 2, "Baz", 0.7)),
        siteBySeq = Map(2L -> Site(2, "Sister", "SIS", "sister.example")),
        currentSiteSeq = 1,
        highScoredTerms = Map.empty,
      )(written)

      assert(!markup.contains("Same Wiki"))
      assert(!markup.contains("Sister Wikis"))
      assert(markup === """ 1. <SIS:Baz 0.7 >""")
    }

    "omits group bullets when only same wiki pages exist" in {
      val markup = MacroSimilarPages.formatSimilarPagesMarkup(
        sameSiteSimilarities = Seq(CalculatedCosineSimilarity(1, "Foo", 1, "Bar", 0.8)),
        crossSiteSimilarities = Seq.empty,
        siteBySeq = Map(1L -> Site(1, "Current", "CUR", "current.example")),
        currentSiteSeq = 1,
        highScoredTerms = Map.empty,
      )(written)

      assert(!markup.contains("Same Wiki"))
      assert(!markup.contains("Sister Wikis"))
      assert(markup === """ 1. <Bar 0.8 >""")
    }

    "hands a name to the line as it is, and writes none of it into the markup itself" in {
      val hostile = "a)]][[Html(<b>x</b>)]]"
      val markup = MacroSimilarPages.formatSimilarPagesMarkup(
        sameSiteSimilarities = Seq.empty,
        crossSiteSimilarities = Seq(CalculatedCosineSimilarity(1, "Foo", 2, hostile, 0.7)),
        siteBySeq = Map(2L -> Site(2, "Sister", "SIS", "sister.example")),
        currentSiteSeq = 1,
        highScoredTerms = Map.empty,
      )(line => { assert(line.link === s"SIS:$hostile"); "KEY" })

      assert(markup === " 1. KEY")
    }
  }
}
