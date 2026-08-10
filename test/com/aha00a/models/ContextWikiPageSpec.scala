package com.aha00a.models

import com.aha00a.tests.provider.EmptyContextWikiPage
import org.scalatest.freespec.AnyFreeSpec

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
  }
}
