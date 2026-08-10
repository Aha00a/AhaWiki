package com.aha00a.models

import com.aha00a.tests.provider.EmptyContextWikiPage
import logics.wikis.RenderingMode
import models.ContextWikiPage
import org.scalatest.freespec.AnyFreeSpec

import java.time.LocalDate

/**
 * The include stack, which had no caller for fourteen months and so no test.
 *
 * `name` returning the bottom of the stack instead of the top is what that cost: harmless while
 * nothing pushed, and wrong for every attachment key, edit link and backlink the moment
 * something did.
 */
class ContextWikiPageSpec extends AnyFreeSpec with EmptyContextWikiPage {
  private val requested = contextWikiPage.name

  "a page the reader requested" - {
    "is not included" in {
      assert(!contextWikiPage.isIncluded)
    }

    "is its own root" in {
      assert(contextWikiPage.name === contextWikiPage.nameRoot)
    }
  }

  "push" - {
    "makes the included page the one being rendered" in {
      assert(contextWikiPage.push("Inner").name === "Inner")
    }

    "leaves the requested page as the root" in {
      assert(contextWikiPage.push("Inner").nameRoot === requested)
    }

    "marks the page as included" in {
      assert(contextWikiPage.push("Inner").isIncluded)
    }

    "stacks, innermost first" in {
      val nested = contextWikiPage.push("Middle").push("Inner")

      assert(nested.seqName === Seq("Inner", "Middle", requested))
      assert(nested.name === "Inner")
      assert(nested.nameRoot === requested)
    }

    "leaves the context it was pushed from alone" in {
      contextWikiPage.push("Inner")

      assert(contextWikiPage.seqName === Seq(requested))
    }

    "carries the rendering mode down" in {
      assert(contextWikiPage.push("Inner").renderingMode === contextWikiPage.renderingMode)
    }

    "carries the date down, so an included page does not silently jump to today" in {
      val yesterday = LocalDate.of(2020, 1, 3)

      assert(contextWikiPage.at(yesterday).push("Inner").localDateNow === yesterday)
    }

    "reuses what the including page already worked out about the site" in {
      var computed = 0
      val outer = new ContextWikiPage(Seq("Outer"), RenderingMode.Normal) {
        override lazy val setPageNameByPermission: Set[String] = { computed += 1; Set("Inner") }
      }

      val nested = outer.push("Middle").push("Inner")

      assert(outer.setPageNameByPermission === Set("Inner"))
      assert(nested.setPageNameByPermission === Set("Inner"))
      assert(computed === 1)
    }
  }
}
