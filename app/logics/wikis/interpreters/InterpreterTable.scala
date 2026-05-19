package logics.wikis.interpreters

import java.io.StringReader

import com.aha00a.commons.Implicits._
import com.aha00a.commons.utils.Using
import models.{PageContent, ContextWikiPage}
import org.supercsv.io.CsvListReader
import org.supercsv.prefs.CsvPreference

import scala.jdk.CollectionConverters._
import scala.util.matching.Regex

object InterpreterTable extends TraitInterpreter {

  import models.tables.CalculatedLink

  val regexShebang: Regex = """([ct]sv)(?:\s+(\d+)(?:\s+(\d+))?)?(?:\s+(.+))?""".r

  private val regexCssClassToken: Regex = """^[A-Za-z0-9_-]+$""".r
  private val regexBlockHeading: Regex = """^(={1,6})(>)?\s+.+""".r
  private val regexBlockHr: Regex = """^[-=]{4,}$""".r
  private val regexBlockList: Regex = """^\s+([*-]|\S+\.)\s+.+""".r
  private val regexBlockHtmlLine: Regex = """^\s*</?(Columns|div|span)(?:\s+[^>]*)?>\s*$""".r
  private val defaultCellValue = ""

  case class Shebang(csvPreference:CsvPreference, thRow:Int, thColumn:Int, classes:Option[String]) {
    def getClasses: String = (Seq("simpleTable", "tablesorter") ++ classes.toSeq.flatMap(_.split("""\s+""")))
      .distinct
      .mkString(" ")
  }

  private def sanitizeClasses(classes: String): Option[String] = Option(classes)
    .map(_.trim)
    .filter(_.nonEmpty)
    .map(_.split("""\s+""").toSeq
      .filter(token => regexCssClassToken.matches(token))
      .distinct
      .mkString(" ")
    )
    .filter(_.nonEmpty)

  private def csvPreferenceFrom(format: String): Option[CsvPreference] = format match {
    case "tsv" => Some(CsvPreference.TAB_PREFERENCE)
    case "csv" => Some(CsvPreference.STANDARD_PREFERENCE)
    case _ => None
  }

  private def normalizeCell(value: String): String = Option(value).getOrElse(defaultCellValue)

  private def canRenderCellInline(value: String): Boolean = {
    value.nonEmpty &&
      !value.exists(c => c == '\n' || c == '\r') &&
      !value.contains("[[") &&
      !value.contains("`") &&
      !regexBlockHeading.matches(value) &&
      !regexBlockHr.matches(value) &&
      !regexBlockList.matches(value) &&
      !regexBlockHtmlLine.matches(value)
  }

  private def cellToHtml(value: String)(implicit wikiContext: ContextWikiPage): String = {
    val cell = normalizeCell(value)
    if (cell.isEmpty) {
      "<div></div>"
    } else if (canRenderCellInline(cell)) {
      s"<div><p>${InterpreterWiki.inlineToHtmlString(cell)}</p></div>"
    } else {
      InterpreterWiki.toHtmlString(cell)
    }
  }

  private def cellToSeqLink(value: String)(implicit wikiContext: ContextWikiPage): Seq[CalculatedLink] = {
    val cell = normalizeCell(value)
    if (cell.isEmpty) {
      Seq()
    } else if (canRenderCellInline(cell)) {
      InterpreterWiki.extractLinkMarkup(cell).map(_.toLink(wikiContext.name)).filterNot(_.dst.startsWith("[")).toSeq
    } else {
      InterpreterWiki.toSeqLink(cell)
    }
  }

  def parseShebang(argument:Seq[String]): Option[Shebang] = argument.mkString(" ") match {
    case regexShebang(format, thRow, thColumn, classes) =>
      csvPreferenceFrom(format).map(csvPreference =>
        Shebang(csvPreference, thRow.toIntOrZero, thColumn.toIntOrZero, sanitizeClasses(classes))
      )
    case _ => None
  }

  private def withShebangAndRows[T](content: String)(f: (Shebang, Seq[Seq[String]]) => T): Option[T] = {
    val pageContent: PageContent = PageContent(content)
    parseShebang(pageContent.argument).map(shebang => {
      Using(new CsvListReader(new StringReader(pageContent.content), shebang.csvPreference)) { listReader =>
        f(shebang, convert(listReader))
      }
    })
  }

  override def toHtmlString(content: String)(implicit wikiContext:ContextWikiPage): String = {
    withShebangAndRows(content) { (shebang, rows) =>
      val theadBuilder = new StringBuilder
      val tbodyBuilder = new StringBuilder
      val cellHtmlCache = scala.collection.mutable.HashMap.empty[String, String]
      def cachedCellToHtml(cell: String): String = cellHtmlCache.getOrElseUpdate(normalizeCell(cell), cellToHtml(cell))

      rows.zipWithIndex.foreach { case (row, rowIndex) =>
        val rowBuilder = new StringBuilder
        row.zipWithIndex.foreach { case (cell, columnIndex) =>
          if (rowIndex < shebang.thRow || columnIndex < shebang.thColumn) {
            rowBuilder.append("<th>").append(cachedCellToHtml(cell)).append("</th>")
          } else {
            rowBuilder.append("<td>").append(cachedCellToHtml(cell)).append("</td>")
          }
        }

        if (rowIndex < shebang.thRow) {
          if (theadBuilder.nonEmpty) theadBuilder.append("\n")
          theadBuilder.append("<tr>").append(rowBuilder).append("</tr>")
        } else {
          if (tbodyBuilder.nonEmpty) tbodyBuilder.append("\n")
          tbodyBuilder.append("<tr>").append(rowBuilder).append("</tr>")
        }
      }

      val thead = theadBuilder.toString
      val tbody = tbodyBuilder.toString
      if(thead.isEmpty)
        s"""<table class="InterpreterTable ${shebang.getClasses}"><tbody>$tbody</tbody></table>"""
      else
        s"""<table class="InterpreterTable ${shebang.getClasses}"><thead>$thead</thead><tbody>$tbody</tbody></table>"""
    }.getOrElse("""Error: invalid table options. Use <code>#!table tsv [thRow] [thColumn] [classes]</code>.""")
  }

  def convert(reader: CsvListReader): Seq[Seq[String]] = {
    Iterator.continually(reader.read())
      .takeWhile(_ != null)
      .map(_.asScala.toSeq)
      .toSeq
  }

  override def toSeqLink(content: String)(implicit wikiContext: ContextWikiPage): Seq[CalculatedLink] = {
    withShebangAndRows(content) { (_, rows) =>
      rows.flatMap(_.flatMap(cellToSeqLink))
    }.getOrElse(Seq())
  }
}
