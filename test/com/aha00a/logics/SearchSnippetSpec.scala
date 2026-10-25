package com.aha00a.logics

import logics.SearchSnippet
import org.scalatest.freespec.AnyFreeSpec

class SearchSnippetSpec extends AnyFreeSpec {
  "the snippet" - {
    "centres on the first hit and marks both cut sides" in {
      val text = "a" * 200 + " needle " + "b" * 200
      val s = SearchSnippet.of(text, "needle", 10)
      assert(s.startsWith("…") && s.endsWith("…"))
      assert(s.contains("needle"))
      assert(s.length < 50)
    }

    "finds the hit whatever its case" in {
      assert(SearchSnippet.of("Some Needle here", "needle", 100).contains("Needle"))
    }

    "has no ellipsis on a side that was not cut" in {
      assert(SearchSnippet.of("needle at the start of a short page", "needle", 100) === "needle at the start of a short page")
    }

    "collapses runs of whitespace and newlines" in {
      assert(SearchSnippet.of("one\n\n  two\tthree needle", "needle", 100) === "one two three needle")
    }

    "falls back to the start of the page when the hit is only in the name" in {
      val text = "x" * 500
      val s = SearchSnippet.of(text, "title-only", 10)
      assert(s.startsWith("xxxx") && s.endsWith("…") && s.length <= 21)
    }

    "answers an empty string for an empty page" in {
      assert(SearchSnippet.of("", "q") === "")
    }
  }
}
