package com.aha00a.logics.wikis

import com.aha00a.tests.provider.EmptyContextWikiPage
import logics.wikis.PageSection
import org.scalatest.freespec.AnyFreeSpec

/**
 * Cutting one section out of a page, for `[[Include(Page#Heading)]]`.
 *
 * What a section is -- the heading and what is under it, to the next heading of the same or a
 * higher level -- and the one trap: `== ` at the start of a line inside a `[[[ ... ]]]` block is text
 * the author wrote, not a heading.
 */
class PageSectionSpec extends AnyFreeSpec with EmptyContextWikiPage {
  private val page =
    """= Title
      |intro
      |== Usage
      |use it
      |=== Examples
      |an example
      |== Notes
      |a note
      |== Usage
      |a second heading with the same title
      |""".stripMargin

  "a section" - {
    "is the heading and what is under it, up to the next heading of its level" in {
      assert(PageSection.extract(page, "Notes") === Some("== Notes\na note"))
    }

    "takes its sub-sections along" in {
      assert(PageSection.extract(page, "Usage") === Some("== Usage\nuse it\n=== Examples\nan example"))
    }

    "stops at the next heading of a higher level, not only the same one" in {
      assert(PageSection.extract(page, "Examples") === Some("=== Examples\nan example"))
    }

    "of the top heading runs to the end of the page" in {
      assert(PageSection.extract(page, "Title").exists(_.endsWith("a second heading with the same title\n")))
    }

    "is the first one, when two headings share a title" in {
      assert(PageSection.extract(page, "Usage").exists(!_.contains("a second heading")))
    }

    "is found by its title trimmed" in {
      assert(PageSection.extract(page, "  Notes ") === Some("== Notes\na note"))
    }

    "is None when there is no such heading" in {
      assert(PageSection.extract(page, "Nope") === None)
    }

    "is not found by the text of a line that is not a heading" in {
      assert(PageSection.extract(page, "intro") === None)
    }
  }

  "a block is not searched for headings" - {
    val withBlock =
      """== Real
        |[[[#!Text
        |== Fake
        |inside the block
        |]]]
        |after the block
        |== Next
        |""".stripMargin

    "a heading inside a block is not found" in {
      assert(PageSection.extract(withBlock, "Fake") === None)
    }

    "a heading inside a block does not end the section around it" in {
      assert(PageSection.extract(withBlock, "Real") === Some("== Real\n[[[#!Text\n== Fake\ninside the block\n]]]\nafter the block"))
    }
  }
}
