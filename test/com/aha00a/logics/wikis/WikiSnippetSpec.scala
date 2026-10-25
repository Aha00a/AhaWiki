package com.aha00a.logics.wikis

import logics.wikis.WikiSnippet
import org.scalatest.freespec.AnyFreeSpec

/**
 * The page a reader sees for a name that does not exist, and the templates it offers.
 *
 * A blank line in a bullet list ends the list, so the one thing worth pinning is that offering no
 * templates leaves the list exactly as it was, and offering some adds items inside it.
 */
class WikiSnippetSpec extends AnyFreeSpec {
  "without templates" - {
    val text = WikiSnippet.notFound("Missing")

    "the list is one block: no blank line between its items" in {
      val items = text.linesIterator.dropWhile(!_.startsWith("== Possible actions")).drop(1).toSeq
      assert(items.nonEmpty && items.forall(_.startsWith(" * ")), items.mkString("\n"))
    }

    "still offers to create the page" in {
      assert(text.contains("""create page</a>"""))
      assert(!text.contains("create from template"))
    }
  }

  "with templates" - {
    val text = WikiSnippet.notFound("Missing", Seq("Template:Person", "Template:Restaurant Review"))
    val items = text.linesIterator.dropWhile(!_.startsWith("== Possible actions")).drop(1).toSeq

    "each is an item in the same list" in {
      assert(items.forall(_.startsWith(" * ")), items.mkString("\n"))
      assert(items.count(_.contains("create from template")) === 2)
    }

    "links to the editor on this page with the template named, encoded" in {
      assert(text.contains("""href="?action=edit&amp;template=Template%3APerson""""))
    }

    "shows the template's name without the prefix" in {
      assert(text.contains("create from template: Person</a>"))
      assert(text.contains("create from template: Restaurant Review</a>"))
    }

    "keeps a ) or markup in a template's name from ending the macro or opening a tag" in {
      val hostile = WikiSnippet.notFound("Missing", Seq("Template:A) <b>x</b>"))
      val line = hostile.linesIterator.find(_.contains("create from template")).get
      assert(line.endsWith(")]]"), line)
      assert(!line.contains("<b>"), line)
      assert(line.count(_ == ')') === 1, line)
    }
  }
}
