package logics.wikis

import com.aha00a.commons.Implicits._
import logics.wikis.macros.MacroCopyable
import models.ContextWikiPage

import scala.collection.mutable.ArrayBuffer

class ExtractConvertInjectBackQuote() extends ExtractConvertInject {
  // Backticks are pulled before [[[blocks]]] are, so a backtick can shield a [[[ or {{var}} written
  // inside it. But a backtick written INSIDE a block (#!Text, #!Vim, WikiSyntaxPreview Raw) must be
  // left as the literal character the block shows -- and left unchanged between renders, so a Vim
  // block's md5 cache key (built from its body) does not shift every time. So blocks and backticks
  // are read in one left-to-right pass: whichever of a backtick span or a [[[...]]] opens first owns
  // its extent, and backticks are pulled only outside the blocks. The blocks themselves are still
  // pulled afterwards, by ExtractConvertInjectInterpreter; a [[[ hidden inside a pulled backtick is
  // no longer in the text by then, so it opens no block.
  override def extract(s: String): String =
    partitionByBlock(s).map {
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

  // Split s into (isBlock, text) runs. A [[[...]]] region is one block run, returned verbatim so its
  // interior (including any backtick) is untouched. A backtick span is stepped over while scanning
  // -- a [[[ inside it opens no block -- and left in the surrounding non-block text, where
  // extractBackQuotes pulls it exactly as it did when the whole document was scanned at once. A
  // backtick span is single-line (the extract regexes forbid a newline), so it is sought only within
  // the current line; a [[[ pairs with the first following ]]], as ExtractConvertInjectInterpreter does.
  private def partitionByBlock(s: String): Seq[(Boolean, String)] = {
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
          out += ((false, sb.toString)); sb.clear()
          out += ((true, s.substring(i, close + 3)))
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
