package com.aha00a.logics.wikis.macros

import com.aha00a.tests.provider.EmptyContextWikiPage
import logics.wikis.macros.MacroInlineDays
import org.scalatest.freespec.AnyFreeSpec

/**
 * Pushing a spliced page's headings down a level.
 *
 * A month page supplies the heading for each day, so the day's own headings have to sit under
 * it. The shift used to run over the raw markup, where `== ` at the start of a line inside a
 * `[[[ ... ]]]` block — a shell transcript, a diff — is text the author wrote rather than a
 * heading, and would have gained an `=` it never had.
 */
class MacroInlineDaysSpec extends AnyFreeSpec with EmptyContextWikiPage {

  "oneLevelDeeper" - {
    "pushes a heading down one level" in {
      assert(MacroInlineDays.oneLevelDeeper("== Lunch") === "=== Lunch")
    }

    "pushes every level, keeping the depth between them" in {
      val shifted = MacroInlineDays.oneLevelDeeper("= One\n== Two\n====== Six")

      assert(shifted === "== One\n=== Two\n======= Six")
    }

    "leaves text that only looks like a heading alone" in {
      assert(MacroInlineDays.oneLevelDeeper("not == a heading") === "not == a heading")
      assert(MacroInlineDays.oneLevelDeeper("==missing space") === "==missing space")
    }

    "does not touch a heading-shaped line inside an interpreter block" in {
      val markup =
        """== Ate out
          |[[[#!Vim diff
          |== 8 <     the old line
          |== 9 >     the new line
          |]]]
          |== Went home""".stripMargin

      val shifted = MacroInlineDays.oneLevelDeeper(markup)

      assert(shifted.contains("=== Ate out"))
      assert(shifted.contains("=== Went home"))
      assert(shifted.contains("== 8 <     the old line"))
      assert(!shifted.contains("=== 8 <"))
    }

    "puts the block back exactly as it was, delimiters included" in {
      val markup = "[[[#!Html\n<p>hi</p>\n]]]"

      assert(MacroInlineDays.oneLevelDeeper(markup) === markup)
    }

    "handles more than one block" in {
      val markup = "[[[#!Text\n== a\n]]]\n== between\n[[[#!Text\n== b\n]]]"

      val shifted = MacroInlineDays.oneLevelDeeper(markup)

      assert(shifted === "[[[#!Text\n== a\n]]]\n=== between\n[[[#!Text\n== b\n]]]")
    }

    "leaves an empty body alone" in {
      assert(MacroInlineDays.oneLevelDeeper("") === "")
    }
  }

  "registration" - {
    "is named after what it does, not after Include" in {
      assert(MacroInlineDays.name === "InlineDays")
    }
  }
}
