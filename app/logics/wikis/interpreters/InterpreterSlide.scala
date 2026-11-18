package logics.wikis.interpreters

import models.{ContextWikiPage, PageContent}

// A presentation deck. It shares the whole content model with InterpreterPaper (split on `----`,
// #!var variables, per-chunk render, {{pageNo}} / {{pageTotal}}) through PaperSlideCore, and differs
// only in presentation: on screen it shows one 16:9 slide at a time with keyboard/click navigation
// and fullscreen (public/js/AhaWiki.Slide.js, styled in app/assets/_slide.less), and in print it
// falls back to a static handout -- every slide shown, one per page -- which is what a slide deck
// printed to PDF is anyway. Animation and fragments are screen-only, so the printed handout is
// always the fully-revealed static form.
//
// Below a `???` line a slide holds its speaker notes (2026-11-18). They go in an <aside> the audience
// never sees: on screen the script shows them in a panel under the slide (N) or in a presenter
// window that follows the deck (P); the stacked no-script fallback and print leave them out.
object InterpreterSlide extends TraitInterpreter {

  import models.tables.CalculatedLink

  override def toHtmlString(content: String)(implicit wikiContext: ContextWikiPage): String = {
    val pageContent: PageContent = PageContent(content)
    val (eciv, chunks) = PaperSlideCore.render(content, notes = true)

    val cssClass = pageContent.argument.lift(0).filter(_.nonEmpty)
      .getOrElse(eciv.variables.getOrElse("class", ""))

    val total = chunks.length
    val slides = chunks.map { c =>
      val orient = if (c.isLandscape) " landscape" else if (c.isPortrait) " portrait" else ""
      s"""<section class="slide$orient" data-index="${c.index}">
         |  <div class="slideNumber">${c.pageNo} / ${c.pageTotal}</div>
         |  <div class="slideContent"><div>${c.html}</div></div>${c.notesHtml.map(n => "\n  <aside class=\"slideNotes\">" + n + "</aside>").getOrElse("")}
         |</section>""".stripMargin
    }

    // The notes and presenter-window buttons only where a slide has notes to read.
    val notesButtons =
      if (!chunks.exists(_.notesHtml.isDefined)) ""
      else
        """
          |      <button type="button" class="slideNotesToggle" title="Speaker notes (N)" aria-label="Speaker notes" aria-pressed="false">&#9998;</button>
          |      <button type="button" class="slidePresenter" title="Presenter window (P)" aria-label="Presenter window">&#9707;</button>""".stripMargin

    // tabindex makes the deck focusable so keyboard navigation is scoped to it (a click focuses it),
    // rather than hijacking the arrow keys of the whole page. Without JS the deck degrades to the
    // slides stacked and readable; AhaWiki.Slide.js adds the `presenting` class to switch to one at
    // a time. data-total lets the script show the counter before it has measured anything.
    s"""<div class="slideDeck $cssClass" tabindex="0" data-total="$total">
       |${slides.mkString("\n")}
       |  <div class="slideChrome">
       |    <div class="slideChromeGroup">
       |      <button type="button" class="slideFilmstrip" title="Filmstrip (L)" aria-label="Filmstrip" aria-pressed="false">&#9636;</button>
       |      <button type="button" class="slideOverview" title="Overview (O)" aria-label="Overview" aria-pressed="false">&#9638;</button>
       |    </div>
       |    <div class="slideChromeGroup">
       |      <button type="button" class="slidePrev" title="Previous slide (&#8592;)" aria-label="Previous slide">&#8249;</button>
       |      <span class="slideCounter"><span class="slideCurrent">1</span> / $total</span>
       |      <button type="button" class="slideNext" title="Next slide (&#8594;)" aria-label="Next slide">&#8250;</button>
       |    </div>
       |    <div class="slideChromeGroup">
       |      <span class="slideTimer" hidden></span>$notesButtons
       |      <button type="button" class="slideFullscreen" title="Fullscreen (F)" aria-label="Fullscreen" aria-pressed="false">&#9974;</button>
       |    </div>
       |  </div>
       |</div>""".stripMargin
  }

  override def toSeqLink(content: String)(implicit wikiContext: ContextWikiPage): Seq[CalculatedLink] =
    PaperSlideCore.toSeqLink(content)
}
