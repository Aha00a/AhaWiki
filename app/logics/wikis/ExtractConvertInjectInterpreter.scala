package logics.wikis

import com.aha00a.commons.utils.ShebangUtil
import logics.wikis.interpreters.Interpreters
import models.ContextWikiPage

class ExtractConvertInjectInterpreter() extends ExtractConvertInject {
  private case class InterpreterChunk(lineStart: Int, lineEnd: Int)
  private var extractedLineToOriginalLine: Vector[Int] = Vector(1)

  def originalLineNumber(extractedLineNumber: Int): Int = {
    if (extractedLineNumber <= 0) 1
    else extractedLineToOriginalLine.lift(extractedLineNumber - 1).getOrElse(extractedLineToOriginalLine.lastOption.getOrElse(1))
  }

  private val chunkMap = scala.collection.mutable.Map.empty[String, InterpreterChunk]

  import models.tables.CalculatedLink
  import models.tables.CalculatedSchemaOrg

  override def extract(s: String): String = {
    if (s == null || !s.contains("[[[") || !s.contains("]]]")) {
      // No interpreter blocks: extracted content == original content (identity mapping).
      // Map line N → N so that originalLineNumber(N) == N for all lines.
      val lineCount = if (s == null || s.isEmpty) 1 else s.count(_ == '\n') + 1
      extractedLineToOriginalLine = (1 to lineCount).toVector
      s
    } else {
      val open = "[[["
      val close = "]]]"
      val builder = new StringBuilder
      val lineMap = scala.collection.mutable.ArrayBuffer[Int]()
      var currentOriginalLine = 1

      def appendText(text: String): Unit = {
        if (text.nonEmpty) {
          if (lineMap.isEmpty) lineMap += currentOriginalLine
          text.foreach { c =>
            builder.append(c)
            if (c == '\n') {
              currentOriginalLine += 1
              lineMap += currentOriginalLine
            }
          }
        }
      }

      var cursor = 0
      var openIndex = s.indexOf(open, cursor)

      while (openIndex >= 0) {
        val closeIndex = s.indexOf(close, openIndex + open.length)
        if (closeIndex < 0) {
          appendText(s.substring(cursor))
          extractedLineToOriginalLine = if (lineMap.nonEmpty) lineMap.toVector else Vector(1)
          return builder.toString()
        }

        appendText(s.substring(cursor, openIndex))

        val uniqueKey = getUniqueKey
        val body = s.substring(openIndex + open.length, closeIndex)

        arrayBuffer += uniqueKey -> body
        chunkMap += uniqueKey -> InterpreterChunk(
          lineStart = lineNumber(s, openIndex),
          lineEnd = lineNumber(s, closeIndex + close.length - 1)
        )

        appendText(uniqueKey)
        currentOriginalLine += body.count(_ == '\n')

        cursor = closeIndex + close.length
        openIndex = s.indexOf(open, cursor)
      }

      appendText(s.substring(cursor))
      extractedLineToOriginalLine = if (lineMap.nonEmpty) lineMap.toVector else Vector(1)
      builder.toString()
    }
  }

  override def convert(s: String)(implicit wikiContext: ContextWikiPage): String = Interpreters.toHtmlString(ShebangUtil.addWhenNotExist(s, "text"))

  override def inject(s: String)(implicit wikiContext: ContextWikiPage): String = {
    var result = s
    val revision = PartialEdit.revision
    for ((key, value) <- arrayBuffer) {
      val converted = Interpreters.toHtmlString(ShebangUtil.addWhenNotExist(value, "text"))
      val maybeInterpreter = Interpreters.getInterpreter(value)

      val withMeta = chunkMap.get(key) match {
        case Some(chunk) =>
          val lineEndExclusive = chunk.lineEnd + 1
          val editUrl = PartialEdit.editUrl(revision, chunk.lineStart, lineEndExclusive)
          val editTitle = s"Edit (r$revision, L${chunk.lineStart}-L${lineEndExclusive - 1})"
          s"""<div class="InterpreterRenderMetaWrapper ${maybeInterpreter.map(_.name).getOrElse("")}" style="position: relative;"
             |  data-edit-link="$editUrl"
             |  data-line-start="${chunk.lineStart}"
             |  data-line-end="$lineEndExclusive"
             |  data-edit-title="$editTitle">
             |  <div class="InterpreterRenderContent">$converted</div>
             |</div>""".stripMargin
        case None =>
          converted
      }
      result = result.replace(key, withMeta)
    }
    result
  }

  def extractLink()(implicit wikiContext: ContextWikiPage): Seq[CalculatedLink] = {
    arrayBuffer.map(_._2).flatMap(c => Interpreters.toSeqLink(c)).toSeq
  }

  def extractSchemaOrg()(implicit wikiContext: ContextWikiPage): Seq[CalculatedSchemaOrg] = {
    arrayBuffer.map(_._2).flatMap(c => Interpreters.toSeqSchemaOrg(c)).toSeq
  }

  private def lineNumber(s: String, charIndexInclusive: Int): Int = {
    if (charIndexInclusive <= 0) {
      1
    } else {
      s.substring(0, charIndexInclusive + 1).count(_ == '\n') + 1
    }
  }

}
