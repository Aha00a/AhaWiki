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

    // Speaker notes: below a `???` line, kept out of the slide's content in an <aside>, and the
    // notes buttons appear only on a deck that has some.
    {
      val html = Interpreters.toHtmlString("#!Slide\n= Title\nshown-marker\n???\nnotes-marker\n----\nplain")
      assertEquals(count(html, "<aside class=\"slideNotes\">"), 1)
      val aside = html.substring(html.indexOf("<aside class=\"slideNotes\">"))
      assertEquals(aside.contains("notes-marker"), true)
      val content = html.substring(html.indexOf("slideContent"), html.indexOf("<aside"))
      assertEquals(content.contains("shown-marker"), true)
      assertEquals(content.contains("notes-marker"), false)
      assertEquals(html.contains("???"), false)
      assertEquals(html.contains("slideNotesToggle"), true)
      assertEquals(html.contains("slidePresenter"), true)
      assertEquals(count(html, "<section class=\"slide"), 2)

      val without = Interpreters.toHtmlString("#!Slide\none\n----\ntwo")
      assertEquals(without.contains("slideNotes"), false)
      assertEquals(without.contains("slidePresenter"), false)
    }

    // Notes may be a block of their own -- a Markdown script, as the EWS deck keeps it. That works
    // when the deck is the whole page; inside an inline [[[#!Slide]]] the first ]]] would close it.
    {
      val html = Interpreters.toHtmlString("#!Slide\n= Cover\n???\n[[[#!Markdown\n### 1. First\n\nsay **this**\n]]]\n----\npicture\n???\n[[[#!Markdown\n### 2. Second\n]]]")
      assertEquals(count(html, "<section class=\"slide"), 2)
      assertEquals(count(html, "<aside class=\"slideNotes\">"), 2)
      assertEquals(html.contains("<strong>this</strong>"), true)
      assertEquals(html.contains("???"), false)
    }

    // Notes are Slide's alone: in Paper the `???` line is text, and the core leaves it unless asked.
    {
      val (_, chunks) = PaperSlideCore.render("#!Paper\na\n???\nb")
      assertEquals(chunks.head.notesHtml, None)
      assertEquals(chunks.head.html.contains("b"), true)
      val (_, slideChunks) = PaperSlideCore.render("#!Slide\na\n???\nb", notes = true)
      assertEquals(slideChunks.head.notesHtml.exists(_.contains("b")), true)
      assertEquals(slideChunks.head.html.contains("b"), false)
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
