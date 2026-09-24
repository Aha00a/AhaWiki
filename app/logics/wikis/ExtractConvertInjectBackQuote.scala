package logics.wikis

import com.aha00a.commons.Implicits._
import logics.wikis.macros.MacroCopyable
import models.ContextWikiPage

import scala.collection.mutable.ArrayBuffer

class ExtractConvertInjectBackQuote() extends ExtractConvertInject {
  // Backticks are pulled before [[[blocks]]] are, so a backtick can shield a [[[ or {{var}} written
  // inside it. But a block whose body is not wiki text must keep the backtick it was written with.
  // Two kinds:
  //
  //   - What the block SHOWS verbatim -- #!Text, #!Vim, the Raw side of #!WikiSyntaxPreview, and a
  //     bare [[[...]]] which defaults to Text. A code span there is not what the page said, and a
  //     Vim block's cache key (md5 of its body) shifted every render because the body held a fresh
  //     placeholder.
  //   - What the block HANDS ON as source -- #!Kanban, #!Gantt, #!Mermaid, #!Math, #!Html. The body
  //     goes to a client-side consumer or straight into the page, so <code> lands inside a Mermaid
  //     diagram, a MathJax expression or the author's own HTML. Worst is Kanban: the board reads its
  //     source with textContent, which drops the tags, so the next board save wrote the page back
  //     with the backticks silently gone. Its card text is rendered as AhaMark by the client anyway,
  //     which is where a backtick there becomes a code span -- as the author meant.
  //
  // Wiki-like blocks (#!Quote, #!Table, #!Fold, #!Paper, #!Wiki, ...) re-render their body as wiki
  // and turn the backtick into a code span either way, so they are left exactly as before: pulling
  // the backtick here rather than in the nested render keeps their <p> wrapping and line breaks
  // unchanged. #!Markdown is left alone too -- txtmark makes the same code span out of what it is
  // handed. #!Graph and #!Map parse their body as data, where a backtick has never appeared.
  //
  // So the text is read once, left to right: a literal block's [[[...]]] is set aside untouched,
  // everything else (plain text and non-literal blocks) keeps having its backticks pulled. Backtick
  // spans are stepped over while scanning so a [[[ inside a backtick opens no block. The blocks
  // themselves are still pulled afterwards by ExtractConvertInjectInterpreter.
  override def extract(s: String): String =
    partitionLiteralBlocks(s).map {
      case (true, block) => block
      case (false, text) => extractBackQuotes(text)
    }.mkString

  private def extractBackQuotes(s: String): String = {
    val regexDoubleBackquote = """``(.+?)``""".r
    val s1 = regexDoubleBackquote.replaceAllIn(s, _ match {
      case regexDoubleBackquote(body) =>
        val uniqueKey = getUniqueKey
        arrayBuffer += uniqueKey -> MacroCopyable.doToHtmlString(body)
        uniqueKey
      case _ => "error"
    })
    val regexSingleBackquote = """`(.+?)`""".r
    regexSingleBackquote.replaceAllIn(s1, _ match {
      case regexSingleBackquote(body) =>
        val uniqueKey = getUniqueKey
        arrayBuffer += uniqueKey -> s"<code>${body.escapeHtml()}</code>"
        uniqueKey
      case _ => "error"
    })
  }

  private val regexDoubleBackquotePrefix = """``.+?``""".r
  private val regexSingleBackquotePrefix = """`.+?`""".r
  private val literalInterpreters = Set(
    "text", "vim", "wikisyntaxpreview", // shown verbatim
    "kanban", "gantt", "mermaid", "math", "html", // handed on as source
  )

  // The interpreter a [[[block]]] body reads as, matching how Interpreters/ShebangUtil resolve it:
  // a body with no shebang is Text, a body that has only directives (#!read etc.) is Wiki, otherwise
  // the first shebang word. Which names keep their backticks literal, and why, is on the set above.
  private def keepsBackticksLiteral(body: String): Boolean = {
    val name =
      if (!body.trim.startsWith("#!")) "text"
      else {
        val normalized = if (body.startsWith("\n#!")) body.substring(1) else body
        normalized.linesIterator
          .takeWhile(_.startsWith("#!"))
          .map(_.substring(2))
          .filterNot(_.startsWith("read"))
          .filterNot(_.startsWith("write"))
          .filterNot(_.startsWith("redirect"))
          .filterNot(d => d == "var" || d.startsWith("var ") || d.startsWith("var\t"))
          .flatMap(_.split("""\s+"""))
          .find(_.nonEmpty)
          .map(_.toLowerCase)
          .getOrElse("wiki")
      }
    literalInterpreters.contains(name)
  }

  // Split s into (isLiteralBlock, text) runs. A literal block's [[[...]]] is its own run, verbatim,
  // so its interior (backticks included) is untouched. Everything else -- plain text and non-literal
  // blocks -- accumulates into non-block runs where extractBackQuotes pulls backticks exactly as when
  // the whole document was scanned at once. A backtick span is single-line (the extract regexes
  // forbid a newline) and is stepped over so a [[[ inside it opens no block; a [[[ pairs with the
  // first following ]]], as ExtractConvertInjectInterpreter does.
  private def partitionLiteralBlocks(s: String): Seq[(Boolean, String)] = {
    val out = ArrayBuffer[(Boolean, String)]()
    val sb = new StringBuilder
    var i = 0
    val n = s.length
    while (i < n) {
      val c = s.charAt(i)
      if (c == '`') {
        val nl = s.indexOf('\n', i)
        val line = s.substring(i, if (nl < 0) n else nl)
        regexDoubleBackquotePrefix.findPrefixMatchOf(line)
          .orElse(regexSingleBackquotePrefix.findPrefixMatchOf(line)) match {
          case Some(m) => sb.append(line.substring(0, m.end)); i += m.end
          case None => sb.append(c); i += 1
        }
      } else if (c == '[' && s.startsWith("[[[", i)) {
        val close = s.indexOf("]]]", i + 3)
        if (close >= 0) {
          val region = s.substring(i, close + 3)
          if (keepsBackticksLiteral(s.substring(i + 3, close))) {
            out += ((false, sb.toString)); sb.clear()
            out += ((true, region))
          } else {
            sb.append(region) // non-literal block: pulled as before, backticks and all
          }
          i = close + 3
        } else {
          sb.append(c); i += 1
        }
      } else {
        sb.append(c); i += 1
      }
    }
    out += ((false, sb.toString))
    out.toSeq
  }

  override def convert(s: String)(implicit wikiContext: ContextWikiPage): String = s
}
