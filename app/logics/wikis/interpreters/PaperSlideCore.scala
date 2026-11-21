package logics.wikis.interpreters

import logics.wikis.ExtractConvertInjectVariable
import models.{ContextWikiPage, PageContent}

// The content model shared by InterpreterPaper (print / A4 pages) and InterpreterSlide (screen
// deck): seed the #!var variables, split the body on `----`, and render each chunk through
// InterpreterWiki. The two interpreters differ only in how they wrap each rendered chunk and the
// container around them -- Paper into A4 pages with a 6-position header/footer, Slide into a deck of
// <section> slides -- so that wrapping stays in each interpreter and only the splitting/rendering,
// the part that would otherwise be copied, lives here.
object PaperSlideCore {

  import models.tables.CalculatedLink

  private val landscapeMarkerRe = """(?m)^\s*#!landscape[ \t]*$""".r
  private val portraitMarkerRe  = """(?m)^\s*#!portrait[ \t]*$""".r
  // The speaker-notes separator: a line of exactly `???` (remark.js's spelling). Only Slide asks for
  // it; for Paper the line is text like any other.
  private val notesMarkerRe     = """(?m)^\?\?\?[ \t]*$""".r
  // `#!class a b`: classes for this one slide's <section>. Slide's alone, like `???`. `[ \t]*` rather
  // than `\s*` at the start, so the match never takes the line break before it and the slide's
  // lines keep their numbers.
  private val classMarkerRe     = """(?m)^[ \t]*#!(class(?:[ \t][^\n]*)?)$""".r

  // One rendered page/slide: its zero-based index, its 1-based number and the total (the same
  // {{pageNo}} / {{pageTotal}} Paper exposes as variables), the orientation its per-chunk marker
  // asked for, the chunk already rendered to HTML by InterpreterWiki, and -- when the caller is Slide
  // -- what follows a `???` line, rendered the same way, and the classes its `#!class` lines name.
  final case class Chunk(
    index: Int,
    pageNo: Int,
    pageTotal: Int,
    isLandscape: Boolean,
    isPortrait: Boolean,
    html: String,
    notesHtml: Option[String] = None,
    classes: Seq[String] = Nil,
  )

  // Returns the variable holder (so the caller can read class / docId / header-footer variables and
  // re-apply {{pageNo}} while wrapping) and the rendered chunks. On return `pageNo` in the holder is
  // left at the last chunk's number; a caller that resolves {{pageNo}} per chunk must set it again
  // before each applyVariables -- InterpreterPaper does exactly that for its header and footer.
  //noinspection ZeroIndexToHead
  //
  // `slide` turns on the per-slide lines only Slide reads: `???` and `#!class`. On the first slide a
  // `#!class` line that follows `#!Slide` directly is one of the page's directives, not a line of the
  // body, so PageContent hands it over separately and it is added here.
  def render(content: String, slide: Boolean = false)(implicit wikiContext: ContextWikiPage): (ExtractConvertInjectVariable, Seq[Chunk]) = {
    val pageContent: PageContent = PageContent(content)

    // #!var directive variables seed the holder; extract also pulls [[[#!Variable]]] blocks. Both run
    // before the split so {{key}} substitution works on every chunk.
    val eciv = new ExtractConvertInjectVariable()
    eciv.variables ++= pageContent.variables
    val bodyExtracted = eciv.extract(pageContent.content)

    val chunks = bodyExtracted.split("""(?m)^-{4,}$""", -1)
    eciv.variables("pageTotal") = chunks.length.toString

    // Line numbers count from the content, the `#!` lines left out, as a plain wiki page's do: the
    // edit screen adds the directive lines back (Wiki.buildEditFormState). Until 2026-11-21 this
    // started at the number of directive lines, so they were added twice -- every section's Edit on
    // a #!Paper page (where `#!Paper` itself is one) opened lines further down than the section,
    // and the last section's opened the whole page.
    var lineOffset = 0
    val rendered = chunks.zipWithIndex.map { case (chunk, index) =>
      val offset = lineOffset
      lineOffset += chunk.count(_ == '\n')
      eciv.variables("pageNo") = (index + 1).toString
      val isLandscape    = landscapeMarkerRe.findFirstIn(chunk).isDefined
      val isPortrait     = portraitMarkerRe.findFirstIn(chunk).isDefined
      val strippedChunk  = landscapeMarkerRe.replaceAllIn(portraitMarkerRe.replaceAllIn(chunk, ""), "")
      // The first `???` line splits what the audience sees from what the speaker reads. Both halves
      // are padded to their own raw line numbers, so a partial-edit link in the notes opens the
      // notes' lines, not the slide's.
      val (bodyWithClassLines, notesText) = notesMarkerRe.findFirstMatchIn(strippedChunk).filter(_ => slide) match {
        case Some(m) =>
          val linesBefore = strippedChunk.substring(0, m.end).count(_ == '\n')
          (strippedChunk.substring(0, m.start), Some((linesBefore, strippedChunk.substring(m.end))))
        case None => (strippedChunk, None)
      }
      // `#!class` lines above the notes are the slide's. Each is emptied, not removed, so the lines
      // after it keep their numbers.
      val (body, classes) =
        if (!slide) (bodyWithClassLines, Nil)
        else {
          val fromBody = classMarkerRe.findAllMatchIn(bodyWithClassLines).flatMap(m => PageContent.classNames(m.group(1))).toSeq
          val fromDirectives = if (index == 0) pageContent.classes else Nil
          (classMarkerRe.replaceAllIn(bodyWithClassLines, ""), (fromDirectives ++ fromBody).distinct)
        }
      val paddedResolved = "\n" * offset + eciv.applyVariables(body)
      val notesHtml = notesText.map { case (linesBefore, text) =>
        InterpreterWiki.toHtmlString("\n" * (offset + linesBefore) + eciv.applyVariables(text))
      }
      Chunk(index, index + 1, chunks.length, isLandscape, isPortrait, InterpreterWiki.toHtmlString(paddedResolved), notesHtml, classes)
    }

    (eciv, rendered)
  }

  def toSeqLink(content: String)(implicit wikiContext: ContextWikiPage): Seq[CalculatedLink] = {
    val pageContent: PageContent = PageContent(content)
    val eciv = new ExtractConvertInjectVariable()
    eciv.variables ++= pageContent.variables
    val bodyExtracted = eciv.extract(pageContent.content)
    val bodyResolved  = eciv.applyVariables(bodyExtracted)
    InterpreterWiki.toSeqLink(bodyResolved)
  }
}
