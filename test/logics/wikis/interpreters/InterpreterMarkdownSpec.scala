package logics.wikis.interpreters

import models.tables.CalculatedLink
import org.scalatest.freespec.AnyFreeSpec
import provider.EmptyContextWikiPage

class InterpreterMarkdownSpec extends AnyFreeSpec with EmptyContextWikiPage {
  "toSeqLink" - {
    "extracts inline markdown links" in {
      val links = InterpreterMarkdown.toSeqLink("[Google](https://google.com) [FrontPage](/w/FrontPage)")

      assert(links === Seq(
        CalculatedLink(getClass.getName, "https://google.com", "Google"),
        CalculatedLink(getClass.getName, "/w/FrontPage", "FrontPage")
      ))
    }

    "extracts autolinks" in {
      val links = InterpreterMarkdown.toSeqLink("before <https://example.com/path?q=1> after")

      assert(links === Seq(
        CalculatedLink(getClass.getName, "https://example.com/path?q=1", "")
      ))
    }

    "skips image links and invalid destinations" in {
      val links = InterpreterMarkdown.toSeqLink("![image](https://img.example.com/a.png) [empty]()")

      assert(links === Seq())
    }

    "supports angle-bracket destinations with title" in {
      val links = InterpreterMarkdown.toSeqLink("[Aha](<https://aha.example.com/a b> \"title\")")

      assert(links === Seq(
        CalculatedLink(getClass.getName, "https://aha.example.com/a b", "Aha")
      ))
    }
  }
}
