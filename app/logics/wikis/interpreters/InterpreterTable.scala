package logics.wikis.interpreters

import java.io.StringReader
import java.math.{BigDecimal => JBigDecimal, RoundingMode}

import com.aha00a.commons.Implicits._
import com.aha00a.commons.utils.Using
import models.{PageContent, ContextWikiPage}
import org.supercsv.io.CsvListReader
import org.supercsv.prefs.CsvPreference

import scala.jdk.CollectionConverters._
import scala.util.Try
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

  // A cell that holds a number, as a spreadsheet writes one: an optional sign (the Unicode minus
  // too), an optional currency mark, digits with or without thousands separators, an optional
  // fraction, an optional percent sign. "65,438,723", "-7%", "₩1,200.50" and "+3" are numbers;
  // "1,23", "12 000" and "1.234,5" are not -- the comma is a thousands separator here, as it is in
  // the sheets this wiki's tables are pasted from, and in tablesorter, which sorts these columns.
  private val regexNumber: Regex = """^([+\-−])?[$€£¥₩]?((?:\d{1,3}(?:,\d{3})+|\d+)(?:\.\d+)?)\s*%?$""".r

  // What a numeric column may hold besides numbers and stay numeric: nothing, or the mark a
  // spreadsheet leaves for "no value". Any other text makes the column text.
  private val placeholders: Set[String] = Set("", "-", "–", "—", "n/a")

  /**
   * The shebang, parsed: `#!Table tsv [thRow] [thColumn] [options and classes...]`.
   *
   * Among the trailing words, one with `=` is an option and the rest are CSS classes for the
   * table. Until 2026-11-02 a word with `=` was dropped by `sanitizeClasses`, so claiming it as
   * an option changes no table that rendered before. The one option so far is `bar=`, which
   * names the columns that get a data bar: `bar=all` for every numeric column, or `bar=2,3` by
   * 1-based column number, or `bar=Revenue` by header text. A header with a space in it cannot
   * be named -- the shebang is split on whitespace -- so the number is the general form.
   */
  case class Shebang(csvPreference:CsvPreference, thRow:Int, thColumn:Int, classes:Option[String], bars:Seq[String]) {
    def getClasses: String =
      (Seq("wikiTableSimple", "tablesorter") ++ classes.toSeq.flatMap(_.split("""\s+""")).map(normalizeClassToken))
        .distinct
        .mkString(" ")
  }

  private def normalizeClassToken(token: String): String = token match {
    case "simpleTable" => "wikiTableSimple"
    case _ => token
  }

  private def sanitizeClasses(classes: String): Option[String] = Option(classes)
    .map(_.trim)
    .filter(_.nonEmpty)
    .map(_.split("""\s+""").toSeq
      .filter(token => regexCssClassToken.matches(token))
      .map(normalizeClassToken)
      .distinct
      .mkString(" ")
    )
    .filter(_.nonEmpty)

  private def parseBars(classes: String): Seq[String] = Option(classes).toSeq
    .flatMap(_.split("""\s+"""))
    .collect { case s"bar=$columns" => columns.split(",").toSeq.map(_.trim).filter(_.nonEmpty) }
    .flatten

  private def csvPreferenceFrom(format: String): Option[CsvPreference] = format match {
    case "tsv" => Some(CsvPreference.TAB_PREFERENCE)
    case "csv" => Some(CsvPreference.STANDARD_PREFERENCE)
    case _ => None
  }

  private def normalizeCell(value: String): String = Option(value).getOrElse(defaultCellValue)

  /**
   * The number a cell holds, if it holds one. "1,234" is 1234 and "-7%" is -7. A string of digits
   * too long for a Double reads as infinity, which the bar arithmetic would turn into NaN
   * percentages; it is not a number here.
   */
  def parseNumber(cell: String): Option[Double] = normalizeCell(cell).trim match {
    case regexNumber(sign, digits) =>
      Try(digits.replace(",", "").toDouble).toOption
        .filter(v => !v.isInfinite)
        .map(v => if (sign == "-" || sign == "−") -v else v)
    case _ => None
  }

  private def isPlaceholder(cell: String): Boolean = placeholders.contains(normalizeCell(cell).trim.toLowerCase)

  /** A column is numeric when at least one body cell is a number and no body cell is anything else. */
  private def isNumericColumn(bodyCells: Seq[String]): Boolean = {
    val values = bodyCells.filterNot(isPlaceholder)
    values.nonEmpty && values.forall(cell => parseNumber(cell).isDefined)
  }

  /**
   * Where a column's bars start and end, as Excel draws them by default: proportional to the
   * value, from zero. A column with negative values puts zero on a shared axis, and a negative
   * value's bar runs left from it. A column of one repeated value, or of zeros, has no bars.
   */
  private case class BarScale(lo: Double, hi: Double) {
    private val span: Double = hi - lo
    private val axis: Double = (0 - lo) / span * 100

    def style(value: Double): String = {
      val length = value / span * 100
      val (start, end) = if (value >= 0) (axis, axis + length) else (axis + length, axis)
      s"--bar-start:${percent(start)};--bar-end:${percent(end)}"
    }
  }

  private object BarScale {
    def of(values: Seq[Double]): Option[BarScale] = {
      if (values.isEmpty) return None
      val lo = math.min(values.min, 0)
      val hi = math.max(values.max, 0)
      if (hi - lo > 0) Some(BarScale(lo, hi)) else None
    }
  }

  // One decimal, a dot whatever the locale, "100.0%" at the far end and never past it: floating
  // point can put the far end at 100.00000000000001. Only numbers reach style="".
  private def percent(ratio: Double): String =
    JBigDecimal.valueOf(math.max(0, math.min(100, ratio))).setScale(1, RoundingMode.HALF_UP).toPlainString + "%"

  /**
   * What the table knows about its columns before it draws a cell: which columns are numeric, and
   * for those named by `bar=`, the scale their bars are drawn on. Header rows and header columns
   * take no part; a header column of years stays a header.
   */
  private case class Columns(numeric: Set[Int], barScales: Map[Int, BarScale]) {
    def tdOpen(columnIndex: Int, cell: String): String = {
      if (!numeric.contains(columnIndex)) return "<td>"
      parseNumber(cell) match {
        // A placeholder in a numeric column. tablesorter picks a column's parser from the first
        // cell whose text is not empty -- so a column that opens with "n/a" sorted as text, and
        // the numbers under it in the order of their first character. It reads data-text before
        // the cell's text, an empty one included, so the empty attribute makes it skip the cell,
        // and sorts the cell last with the other empties. Number cells carry no data-text: the
        // filter widget matches typed text against the same attribute, and "65,438" has to find
        // the cell that shows it.
        case None => """<td class="numeric" data-text="">"""
        case Some(value) => barScales.get(columnIndex) match {
          case Some(scale) =>
            val negative = if (value < 0) " negative" else ""
            s"""<td class="numeric dataBar$negative" style="${scale.style(value)}">"""
          case None => """<td class="numeric">"""
        }
      }
    }
  }

  private object Columns {
    def of(shebang: Shebang, rows: Seq[Seq[String]]): Columns = {
      val body = rows.drop(shebang.thRow)
      val width = rows.map(_.size).maxOption.getOrElse(0)
      def bodyCells(columnIndex: Int): Seq[String] = body.map(row => normalizeCell(row.getOrElse(columnIndex, defaultCellValue)))
      val numeric = (shebang.thColumn until width).filter(columnIndex => isNumericColumn(bodyCells(columnIndex)))

      // `bar=` names a column by number or by the text of its header -- the first row, when there
      // is a header row. A name that is no column, or a column that is not numeric, draws nothing.
      val header: Seq[String] = if (shebang.thRow > 0) rows.headOption.getOrElse(Seq.empty).map(cell => normalizeCell(cell).trim) else Seq.empty
      val named: Set[Int] = shebang.bars.flatMap {
        case "all" => numeric
        case ref if ref.forall(_.isDigit) => ref.toIntOption.map(_ - 1).toSeq // a number too long for an Int names no column
        case ref => header.indices.filter(columnIndex => header(columnIndex).equalsIgnoreCase(ref))
      }.toSet
      val barScales = numeric.filter(named.contains).flatMap(columnIndex =>
        BarScale.of(bodyCells(columnIndex).flatMap(parseNumber)).map(columnIndex -> _)
      ).toMap

      Columns(numeric.toSet, barScales)
    }
  }

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
        Shebang(csvPreference, thRow.toIntOrZero, thColumn.toIntOrZero, sanitizeClasses(classes), parseBars(classes))
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
      // The cache is keyed by the cell's text, so it holds what the text becomes -- the inner
      // HTML -- and not the <td>, whose class and bar depend on the column the cell is in.
      val cellHtmlCache = scala.collection.mutable.HashMap.empty[String, String]
      def cachedCellToHtml(cell: String): String = cellHtmlCache.getOrElseUpdate(normalizeCell(cell), cellToHtml(cell))
      val columns = Columns.of(shebang, rows)

      rows.zipWithIndex.foreach { case (row, rowIndex) =>
        val rowBuilder = new StringBuilder
        row.zipWithIndex.foreach { case (cell, columnIndex) =>
          if (rowIndex < shebang.thRow || columnIndex < shebang.thColumn) {
            rowBuilder.append("<th>").append(cachedCellToHtml(cell)).append("</th>")
          } else {
            rowBuilder.append(columns.tdOpen(columnIndex, cell)).append(cachedCellToHtml(cell)).append("</td>")
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
    }.getOrElse("""Error: invalid table options. Use <code>#!table tsv [thRow] [thColumn] [bar=all] [classes]</code>.""")
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
