package com.aha00a.logics.wikis

import logics.ScreenLanguage.Language
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
    val text = WikiSnippet.notFound("Missing", Seq.empty, Language.English)

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
    val text = WikiSnippet.notFound("Missing", Seq("Template:Person", "Template:Restaurant Review"), Language.English)
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
      val hostile = WikiSnippet.notFound("Missing", Seq("Template:A) <b>x</b>"), Language.English)
      val line = hostile.linesIterator.find(_.contains("create from template")).get
      assert(line.endsWith(")]]"), line)
      assert(!line.contains("<b>"), line)
      assert(line.count(_ == ')') === 1, line)
    }
  }

  // In the reader's language (FrameText). The texts go into markup as they are, so in either
  // language the page must keep its one heading, its one Html macro per action and its links.
  "for a Korean reader" - {
    val text = WikiSnippet.notFound("없는문서", Seq("Template:Person"), Language.Korean)

    "says so in Korean" in {
      assert(text.contains("이 문서는 아직 없습니다."))
      assert(text.contains("== 할 수 있는 것"))
      assert(text.contains("""rel="nofollow">이 문서 만들기</a>)]]"""))
      assert(text.contains("틀로 만들기: Person</a>)]]"))
    }

    "keeps the search links whole" in {
      assert(text.contains("""["https://google.com/search?q=없는문서" 없는문서] — Google 에서 찾기"""))
      assert(!text.contains("Search ["))
    }
  }
}
