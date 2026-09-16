package com.aha00a.tests.unit

import com.aha00a.tests.TestUtil
import logics.wikis.interpreters.{Interpreters, PaperSlideCore}
import models.ContextWikiPage
import play.api.mvc.{AnyContent, Request}

// InterpreterSlide shares its whole content model with InterpreterPaper through PaperSlideCore: the
// same `----` split, #!var variables, per-chunk render and {{pageNo}} / {{pageTotal}}. It differs
// only in the wrapper -- a deck of <section class="slide"> rather than A4 .page divs. These tests pin
// the split / numbering / marker behaviour, and that moving the split into the shared core left
// InterpreterPaper's output unchanged.
object SlideUnit {
  def run(testUtil: TestUtil)(implicit request: Request[AnyContent], contextWikiPage: ContextWikiPage): Unit = {
    import testUtil.assertEquals

    def count(haystack: String, needle: String): Int =
      if (needle.isEmpty) 0 else haystack.sliding(needle.length).count(_ == needle)

    // A whole-page #!Slide renders one deck with one slide.
    {
      val html = Interpreters.toHtmlString("#!Slide\nhello")
      assertEquals(html.contains("class=\"slideDeck"), true)
      assertEquals(count(html, "<section class=\"slide"), 1)
      assertEquals(html.contains("hello"), true)
      // The nav chrome carries the overview (grid) and filmstrip (left rail) toggles.
      assertEquals(html.contains("slideOverview"), true)
      assertEquals(html.contains("slideFilmstrip"), true)
    }

    // `----` splits into slides; each carries its own pageNo / pageTotal, and the deck knows the total.
    {
      val html = Interpreters.toHtmlString("#!Slide\none\n----\ntwo\n----\nthree")
      assertEquals(count(html, "<section class=\"slide"), 3)
      assertEquals(html.contains("1 / 3"), true)
      assertEquals(html.contains("2 / 3"), true)
      assertEquals(html.contains("3 / 3"), true)
      assertEquals(html.contains("data-total=\"3\""), true)
    }

    // Per-slide orientation markers become classes, exactly as in Paper.
    {
      val html = Interpreters.toHtmlString("#!Slide\nupright\n----\n#!landscape\nwide")
      assertEquals(html.contains("<section class=\"slide landscape\""), true)
      // The marker line itself is stripped, not rendered.
      assertEquals(html.contains("#!landscape"), false)
    }

    // {{pageNo}} is substituted per slide, from the shared variable holder.
    {
      val html = Interpreters.toHtmlString("#!Slide\npage {{pageNo}}\n----\npage {{pageNo}}")
      assertEquals(html.contains("page 1"), true)
      assertEquals(html.contains("page 2"), true)
    }

    // The shared core: render returns one chunk per `----` section, numbered from 1.
    {
      val (_, chunks) = PaperSlideCore.render("#!Slide\na\n----\nb")
      assertEquals(chunks.length, 2)
      assertEquals(chunks.head.pageNo, 1)
      assertEquals(chunks.head.pageTotal, 2)
      assertEquals(chunks(1).pageNo, 2)
      assertEquals(chunks(1).html.contains("b"), true)
    }

    // Guard: moving the split into PaperSlideCore left InterpreterPaper unchanged -- still an A4
    // .paperContent of .page divs with the default footer.
    {
      val html = Interpreters.toHtmlString("#!Paper\na\n----\nb")
      assertEquals(html.contains("class=\"paperContent"), true)
      // The container div only -- `<div class="page` alone would also match pageHeader/Footer/Content.
      assertEquals(count(html, "<div class=\"page\">"), 2)
      assertEquals(html.contains("1 / 2"), true)
      assertEquals(html.contains("2 / 2"), true)
    }
  }
}
