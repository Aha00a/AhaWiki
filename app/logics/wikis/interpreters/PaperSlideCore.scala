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

  // One rendered page/slide: its zero-based index, its 1-based number and the total (the same
  // {{pageNo}} / {{pageTotal}} Paper exposes as variables), the orientation its per-chunk marker
  // asked for, and the chunk already rendered to HTML by InterpreterWiki.
  final case class Chunk(
    index: Int,
    pageNo: Int,
    pageTotal: Int,
    isLandscape: Boolean,
    isPortrait: Boolean,
    html: String,
  )

  // Returns the variable holder (so the caller can read class / docId / header-footer variables and
  // re-apply {{pageNo}} while wrapping) and the rendered chunks. On return `pageNo` in the holder is
  // left at the last chunk's number; a caller that resolves {{pageNo}} per chunk must set it again
  // before each applyVariables -- InterpreterPaper does exactly that for its header and footer.
  //noinspection ZeroIndexToHead
  def render(content: String)(implicit wikiContext: ContextWikiPage): (ExtractConvertInjectVariable, Seq[Chunk]) = {
    val pageContent: PageContent = PageContent(content)

    // #!var directive variables seed the holder; extract also pulls [[[#!Variable]]] blocks. Both run
    // before the split so {{key}} substitution works on every chunk.
    val eciv = new ExtractConvertInjectVariable()
    eciv.variables ++= pageContent.variables
    val bodyExtracted = eciv.extract(pageContent.content)

    val chunks = bodyExtracted.split("""(?m)^-{4,}$""", -1)
    eciv.variables("pageTotal") = chunks.length.toString

    // lineOffset starts at the number of directive lines (#! lines stripped by PageContent), so that
    // padded content line numbers map back to raw-document line numbers.
    var lineOffset = pageContent.directives.length
    val rendered = chunks.zipWithIndex.map { case (chunk, index) =>
      val offset = lineOffset
      lineOffset += chunk.count(_ == '\n')
      eciv.variables("pageNo") = (index + 1).toString
      val isLandscape    = landscapeMarkerRe.findFirstIn(chunk).isDefined
      val isPortrait     = portraitMarkerRe.findFirstIn(chunk).isDefined
      val strippedChunk  = landscapeMarkerRe.replaceAllIn(portraitMarkerRe.replaceAllIn(chunk, ""), "")
      val paddedResolved = "\n" * offset + eciv.applyVariables(strippedChunk)
      Chunk(index, index + 1, chunks.length, isLandscape, isPortrait, InterpreterWiki.toHtmlString(paddedResolved))
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
