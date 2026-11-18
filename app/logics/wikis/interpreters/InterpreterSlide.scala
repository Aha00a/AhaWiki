package logics.wikis.interpreters

import models.{ContextWikiPage, PageContent}
import org.jsoup.nodes.{Comment, Document, Element, Node, TextNode}
import org.jsoup.parser.{ParseSettings, Parser}

import scala.jdk.CollectionConverters._

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
//
// A `#!class a b` line gives one slide classes of its own (2026-11-18), some of which _slide.less
// draws: `dark`, `cover`, `cards` (with `flow` and `stats`). Two layouts need no class -- a paragraph
// alone above the first heading is drawn small, as a label over the title, and a paragraph that is
// only a picture shrinks into the height the slide has left -- and those, and the cards, are marked
// on the HTML here (`layout`), since what they depend on is the shape of the content, which a
// stylesheet cannot read: whether a paragraph holds text besides its picture, or which list items
// a sub-list belongs to.
object InterpreterSlide extends TraitInterpreter {

  import models.tables.CalculatedLink

  override def toHtmlString(content: String)(implicit wikiContext: ContextWikiPage): String = {
    val pageContent: PageContent = PageContent(content)
    val (eciv, chunks) = PaperSlideCore.render(content, slide = true)

    val cssClass = pageContent.argument.lift(0).filter(_.nonEmpty)
      .getOrElse(eciv.variables.getOrElse("class", ""))

    val total = chunks.length
    val slides = chunks.map { c =>
      val orient = if (c.isLandscape) Seq("landscape") else if (c.isPortrait) Seq("portrait") else Nil
      val (html, layoutClasses) = layout(c.html, c.classes)
      val sectionClass = (Seq("slide") ++ orient ++ c.classes ++ layoutClasses).mkString(" ")
      s"""<section class="$sectionClass" data-index="${c.index}">
         |  <div class="slideNumber">${c.pageNo} / ${c.pageTotal}</div>
         |  <div class="slideContent"><div>$html</div></div>${c.notesHtml.map(n => "\n  <aside class=\"slideNotes\">" + n + "</aside>").getOrElse("")}
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

  /**
   * Marks the parts of one slide's HTML that _slide.less lays out, and returns it with the classes
   * the slide's <section> takes for them:
   *
   *  - `slideKicker` on a paragraph that is all there is above the slide's first heading -- the small
   *    label over a title -- and `slideAfterKicker` on that heading.
   *  - `slidePicture` on a paragraph that holds one picture and nothing else, `slideFill` on each
   *    block between it and the slide, and `slideHasPicture` for the section: the blocks become a
   *    column the picture fills, so it shrinks to the height left under the title.
   *  - On a `cards` slide, the first list that is not inside another becomes the cards: `slideCards`
   *    on it, `slideCard` on each item, its own text in a `slideCardTitle`, and the sub-list under
   *    it -- which AhaMark writes as the next sibling of the item, not inside it -- moved inside as
   *    `slideCardBody`.
   *
   * A slide with none of these comes back as it went in, without passing through the parser.
   */
  private[interpreters] def layout(html: String, classes: Seq[String]): (String, Seq[String]) = {
    // Parsed as the body's children, so a <style> or <script> a block writes stays where it is
    // rather than moving to a <head>; preserveCase, so an inline SVG's viewBox is not lower-cased.
    val document = Document.createShell("")
    document.outputSettings().prettyPrint(false)
    val root = document.body()
    Parser.htmlParser().settings(ParseSettings.preserveCase)
      .parseFragmentInput(html, root, "").asScala.toList.foreach(root.appendChild)

    val kicker = markKicker(root)
    val picture = markPictures(root)
    val cards = classes.contains("cards") && markCards(root)

    if (!kicker && !picture && !cards) (html, Nil)
    else (root.html(), if (picture) Seq("slideHasPicture") else Nil)
  }

  private def isBlankNode(node: Node): Boolean = node match {
    case t: TextNode => t.isBlank
    case _: Comment => true
    case _ => false
  }

  private def onlyChild(element: Element): Option[Element] =
    element.childNodes().asScala.filterNot(isBlankNode).toSeq match {
      case Seq(only: Element) => Some(only)
      case _ => None
    }

  // The one picture `element` holds and nothing else, through the wrappers around it: a link, and
  // the <span class="Macro ..."> every macro's output is put in.
  private def pictureIn(element: Element): Option[Element] =
    onlyChild(element).flatMap { child =>
      child.normalName() match {
        case "img" => Some(child)
        case "a" | "span" => pictureIn(child)
        case _ => None
      }
    }

  // A paragraph that is one picture and no text.
  private def isPicture(element: Element): Boolean =
    element.normalName() == "p" && pictureIn(element).isDefined

  // The blocks from `element` up to the slide's root, exclusive at both ends.
  private def blocksBetween(element: Element, root: Element): Seq[Element] =
    element.parents().asScala.takeWhile(_ != root).toSeq

  private def markKicker(root: Element): Boolean = {
    val marked = for {
      heading <- root.select("h1, h2, h3, h4, h5, h6").asScala.headOption
      wrapper <- heading.parents().asScala.find(_.className().startsWith("HeadingWrapper"))
      before <- Option(wrapper.previousElementSibling())
      paragraph <- onlyChild(before)
      if paragraph.normalName() == "p" && paragraph.text().trim.nonEmpty && !isPicture(paragraph)
    } yield {
      paragraph.addClass("slideKicker")
      heading.addClass("slideAfterKicker")
    }
    marked.isDefined
  }

  private def markPictures(root: Element): Boolean = {
    val pictures = root.select("p").asScala.filter { p =>
      isPicture(p) && blocksBetween(p, root).forall(b => b.normalName() == "div" && !b.hasClass("WikiColumns"))
    }
    pictures.foreach { p =>
      p.addClass("slidePicture")
      blocksBetween(p, root).foreach(_.addClass("slideFill"))
    }
    pictures.nonEmpty
  }

  private def markCards(root: Element): Boolean = {
    val lists = Set("ul", "ol")
    val first = root.select("ul, ol").asScala.find { list =>
      !blocksBetween(list, root).exists(b => lists(b.normalName()) || b.normalName() == "li")
    }
    first.foreach { list =>
      list.addClass("slideCards").removeAttr("style")
      var card: Option[Element] = None
      list.children().asScala.toList.foreach { child =>
        if (child.normalName() == "li") {
          val title = new Element("div").addClass("slideCardTitle")
          child.childNodes().asScala.toList.foreach(title.appendChild)
          child.addClass("slideCard").appendChild(title)
          card = Some(child)
        } else if (lists(child.normalName())) {
          card.foreach(_.appendChild(child.addClass("slideCardBody").removeAttr("style")))
        }
      }
    }
    first.isDefined
  }
}
