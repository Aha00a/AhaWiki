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
      // The key list is every deck's, notes or none: a button, since `?` alone is found by no one.
      assertEquals(html.contains("class=\"slideKeys\""), true)
      assertEquals(without.contains("class=\"slideKeys\""), true)
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
      val (_, slideChunks) = PaperSlideCore.render("#!Slide\na\n???\nb", slide = true)
      assertEquals(slideChunks.head.notesHtml.exists(_.contains("b")), true)
      assertEquals(slideChunks.head.html.contains("b"), false)
    }

    def sections(html: String): Seq[String] =
      """<section class="([^"]*)"""".r.findAllMatchIn(html).map(_.group(1)).toSeq

    // `#!class` gives one slide classes of its own; the line itself is not drawn, the names are
    // filtered to what a class attribute can hold, and the lines after it keep their numbers.
    {
      val html = Interpreters.toHtmlString("#!Slide\none\n----\n#!class dark cards \"><b>x</b> ok_2\n== Two")
      assertEquals(sections(html), Seq("slide", "slide dark cards ok_2"))
      assertEquals(html.contains("#!class"), false)
      assertEquals(html.contains("<b>x</b>"), false)
      assertEquals(html.contains("data-line-start=\"4\""), true)
    }

    // On the first slide the line sits right under `#!Slide`, among the page's directives. It is
    // still that slide's, and it does not become the deck's class.
    {
      val html = Interpreters.toHtmlString("#!Slide\n#!class dark cover\n= Cover\n----\nplain")
      assertEquals(sections(html), Seq("slide dark cover", "slide"))
      assertEquals(html.contains("class=\"slideDeck \""), true)
      assertEquals(html.contains("data-line-start=\"1\""), true)
    }

    // A paragraph alone above the first heading is the label over the title.
    {
      val html = Interpreters.toHtmlString("#!Slide\n종합 관제\n== 상황판\n본문")
      assertEquals(html.contains("<p class=\"slideKicker\">종합 관제</p>"), true)
      assertEquals(html.contains("slideAfterKicker"), true)
      assertEquals(html.contains("<p>본문</p>"), true)
      // Not when it is not alone, and not when there is no heading after it.
      assertEquals(Interpreters.toHtmlString("#!Slide\n하나\n\n둘\n== 제목").contains("slideKicker"), false)
      assertEquals(Interpreters.toHtmlString("#!Slide\n하나").contains("slideKicker"), false)
    }

    // A paragraph that is only a picture fills what the slide has left: it, the blocks around it
    // and the slide are marked for _slide.less.
    {
      val html = Interpreters.toHtmlString("#!Slide\n종합 관제\n== 상황판\n[[Image(/a.webp)]]")
      // Inside the <span> every macro's output is put in.
      assertEquals(html.contains("<p class=\"slidePicture\"><span class=\"Macro InlineMacro Image\"><img"), true)
      assertEquals(sections(html), Seq("slide slideHasPicture"))
      assertEquals(html.contains("<div class=\"HeadingWrapper상황판 slideFill\">"), true)
      // A picture that is a link is still a picture.
      assertEquals(Interpreters.toHtmlString("#!Slide\n[[[#!Html\n<p><a href=\"/x\"><img src=\"/a.webp\"></a></p>\n]]]").contains("slidePicture"), true)
      // A picture with words beside it is a paragraph like any other.
      val captioned = Interpreters.toHtmlString("#!Slide\n== 상황판\n[[Image(/a.webp)]] 설명")
      assertEquals(captioned.contains("slidePicture"), false)
      assertEquals(sections(captioned), Seq("slide"))
      // A width given to the picture becomes the most it may take, so the height can still hold it
      // to its proportions; a picture that is not fitted keeps the width as written.
      val sized = Interpreters.toHtmlString("#!Slide\n== 상황판\n[[Image(/a.webp, 93%)]]")
      assertEquals(sized.contains("style=\"max-width: min(100%, 93%)\""), true)
      assertEquals(sized.contains("style=\"width: 93%\""), false)
      assertEquals(Interpreters.toHtmlString("#!Slide\n== 상황판\n[[Image(/a.webp, 93%)]] 설명").contains("style=\"width: 93%\""), true)
    }

    // The words between a picture and its title are the line that says what it shows -- not the
    // label over the title, not words after the picture, and not words with no title over them.
    {
      val html = Interpreters.toHtmlString("#!Slide\n종합 관제\n== 통합 대시보드\n지도에 안 보이는 것을 알립니다\n\n[[Image(/a.webp)]]\n\n그림 뒤")
      assertEquals(html.contains("<p class=\"slideKicker\">종합 관제</p>"), true)
      assertEquals(html.contains("<p class=\"slideLead\">지도에 안 보이는 것을 알립니다</p>"), true)
      assertEquals(html.contains("<p class=\"slidePicture\">"), true)
      assertEquals(html.contains("<p>그림 뒤</p>"), true)
      assertEquals(count(html, "slideLead"), 1)
      // Two paragraphs are two lines of it.
      assertEquals(count(Interpreters.toHtmlString("#!Slide\n== 상황판\n하나\n\n둘\n\n[[Image(/a.webp)]]"), "<p class=\"slideLead\">"), 2)
      assertEquals(Interpreters.toHtmlString("#!Slide\n머리 없는 글\n\n[[Image(/a.webp)]]").contains("slideLead"), false)
      // Words after a list, between two pictures, or on a slide with no picture are the slide's.
      assertEquals(Interpreters.toHtmlString("#!Slide\n== 상황판\n * 목록\n\n목록 뒤\n\n[[Image(/a.webp)]]").contains("slideLead"), false)
      assertEquals(Interpreters.toHtmlString("#!Slide\n== 상황판\n[[Image(/a.webp)]]\n\n사이\n\n[[Image(/b.webp)]]").contains("slideLead"), false)
      assertEquals(Interpreters.toHtmlString("#!Slide\n== 상황판\n요점").contains("slideLead"), false)
    }

    // `cards`: the first list becomes cards, each item's sub-list moved inside it as the body.
    // AhaMark writes a sub-list as the item's next sibling, so without the move a card would end
    // where its title does.
    {
      val html = Interpreters.toHtmlString("#!Slide\n#!class cards flow\n== 흐름\n * 수집\n  * 센서\n * 판단\n  * 임계값\n  * 지속\n * 발령\n\n * 다른 목록")
      assertEquals(sections(html), Seq("slide cards flow"))
      assertEquals(count(html, "class=\"slideCards\""), 1)
      assertEquals(count(html, "<li class=\"slideCard\">"), 3)
      assertEquals(html.contains("<li class=\"slideCard\"><div class=\"slideCardTitle\">수집</div><ul class=\"slideCardBody\">\n<li>센서</li>\n</ul></li>"), true)
      assertEquals(html.contains("<div class=\"slideCardTitle\">판단</div><ul class=\"slideCardBody\">\n<li>임계값</li>\n<li>지속</li>\n</ul></li>"), true)
      assertEquals(html.contains("<li class=\"slideCard\"><div class=\"slideCardTitle\">발령</div></li>"), true)
      // Only the first list.
      assertEquals(html.contains("<li>다른 목록</li>"), true)
      // Without the class, a list is a list.
      assertEquals(Interpreters.toHtmlString("#!Slide\n * 수집\n  * 센서").contains("slideCard"), false)
    }

    // `keynote` is the deck's class, and only a keynote deck links Pretendard.
    {
      val html = Interpreters.toHtmlString("#!Slide keynote\n#!class cover\n= 제목\n----\n둘")
      assertEquals(html.contains("class=\"slideDeck keynote\""), true)
      assertEquals(sections(html), Seq("slide cover", "slide"))
      assertEquals(count(html, "vendor/pretendard/pretendardvariable-dynamic-subset.css"), 1)
      assertEquals(html.indexOf("pretendardvariable") < html.indexOf("class=\"slideDeck"), true)
      assertEquals(Interpreters.toHtmlString("#!Slide\n#!var class keynote\n= 제목").contains("pretendardvariable"), true)
      assertEquals(Interpreters.toHtmlString("#!Slide\n= 제목").contains("pretendardvariable"), false)
    }

    // `backdrop`: the first picture alone in its paragraph leaves the content, without the width it
    // was given, and is drawn behind it; it is not a picture the slide fits. A second one still is.
    {
      val html = Interpreters.toHtmlString("#!Slide\n#!class cover backdrop\n= 제목\n부제\n[[Image(/bg.webp, 93%)]]")
      assertEquals(sections(html), Seq("slide cover backdrop"))
      assertEquals(count(html, "<div class=\"slideBackdrop\"><img"), 1)
      val backdrop = html.substring(html.indexOf("slideBackdrop"), html.indexOf("slideContent"))
      assertEquals(backdrop.contains("/bg.webp"), true)
      assertEquals(backdrop.contains("style="), false)
      val content = html.substring(html.indexOf("slideContent"))
      assertEquals(content.contains("/bg.webp"), false)
      assertEquals(content.contains("<p>부제</p>"), true)
      assertEquals(html.contains("slidePicture"), false)

      val two = Interpreters.toHtmlString("#!Slide\n#!class backdrop\n[[Image(/bg.webp)]]\n== 상황판\n[[Image(/a.webp)]]")
      assertEquals(sections(two), Seq("slide backdrop slideHasPicture"))
      assertEquals(two.substring(two.indexOf("slideBackdrop"), two.indexOf("slideContent")).contains("/bg.webp"), true)
      assertEquals(two.contains("<p class=\"slidePicture\"><span class=\"Macro InlineMacro Image\"><img src=\"/a.webp\""), true)
      // Without the class a picture stays in the content; with it and no picture nothing moves.
      assertEquals(Interpreters.toHtmlString("#!Slide\n[[Image(/bg.webp)]]").contains("slideBackdrop"), false)
      assertEquals(Interpreters.toHtmlString("#!Slide\n#!class backdrop\n= 제목").contains("slideBackdrop"), false)
    }

    // `big` is drawn by the stylesheet and sized by the script: the server only passes the class on.
    {
      val html = Interpreters.toHtmlString("#!Slide\n#!class big\n== 종합 관제\n상황을 보는 화면")
      assertEquals(sections(html), Seq("slide big"))
      assertEquals(html.contains("<p>상황을 보는 화면</p>"), true)
    }

    // `#!class` is Slide's alone: in Paper the line is text, as `???` is.
    {
      val html = Interpreters.toHtmlString("#!Paper\na\n#!class dark\nb")
      assertEquals(html.contains("#!class dark"), true)
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
