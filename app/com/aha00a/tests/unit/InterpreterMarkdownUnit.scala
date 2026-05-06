package com.aha00a.tests.unit

import com.aha00a.tests.TestUtil
import com.aha00a.tests.provider.EmptyContextWikiPage
import logics.wikis.interpreters.InterpreterMarkdown
import models.tables.CalculatedLink

object InterpreterMarkdownUnit extends EmptyContextWikiPage {
  def run(testUtil: TestUtil): Unit = {
    import testUtil.assertEquals

    {
      val links = InterpreterMarkdown.toSeqLink("[Google](https://google.com) [FrontPage](/w/FrontPage)")
      assertEquals(links, Seq(
        CalculatedLink(getClass.getName, "https://google.com", "Google"),
        CalculatedLink(getClass.getName, "/w/FrontPage", "FrontPage")
      ))
    }

    {
      val links = InterpreterMarkdown.toSeqLink("before <https://example.com/path?q=1> after")
      assertEquals(links, Seq(
        CalculatedLink(getClass.getName, "https://example.com/path?q=1", "")
      ))
    }

    {
      val links = InterpreterMarkdown.toSeqLink("![image](https://img.example.com/a.png) [empty]()")
      assertEquals(links, Seq())
    }

    {
      val links = InterpreterMarkdown.toSeqLink("[Aha](<https://aha.example.com/a b> \"title\")")
      assertEquals(links, Seq(
        CalculatedLink(getClass.getName, "https://aha.example.com/a b", "Aha")
      ))
    }
  }
}
