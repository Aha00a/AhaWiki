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

  // A cell that holds a number, as a spreadsheet writes one: an optional sign, an optional
  // currency mark, digits with or without thousands separators, an optional fraction, and an
  // optional unit on the end -- a percent sign, 원, 명, kg, up to eight letters or marks, with
  // or without a space. Accounting's "(1,234)" is a negative. "65,438,723", "-7%", "₩1,200.50",
  // "+3", "35명" and "12.5kg" are numbers; "1,23", "12 000", ".5", "1e5" and "2024년 3월" are not.
  // The comma is a thousands separator, as it is in the sheets these tables are pasted from.
  //
  // What counts here has to sort the same way in the browser: the header of a numeric column
  // names tablesorter's digit parser (thOpen below), which strips everything but digits, the
  // ASCII sign, parentheses and the separators before it reads the value. That is why the
  // Unicode minus (U+2212) is not a sign here -- tablesorter would drop it and sort −3 as 3 --
  // and why a sign and parentheses cannot both appear: "-(1,234)" is text to that parser.
  private val regexNumber: Regex = """^(?:([+\-])|(\())?[$€£¥₩]?((?:\d{1,3}(?:,\d{3})+|\d+)(?:\.\d+)?)(\))?(?:\s*[\p{L}%‰℃°]{1,8})?$""".r

  // What a numeric column may hold besides numbers and stay numeric: nothing, or the mark a
  // spreadsheet leaves for "no value" -- "#N/A" is what Excel pastes for a lookup that found
  // nothing. Any other text makes the column text.
  private val placeholders: Set[String] = Set("", "-", "–", "—", "n/a", "#n/a")

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

  private def sanitizeClasses(tokens: Seq[String]): Option[String] = Some(tokens
    .filter(token => regexCssClassToken.matches(token))
    .map(normalizeClassToken)
    .distinct
    .mkString(" ")
  ).filter(_.nonEmpty)

  // The key is matched case aside, like the interpreter's own name: `Bar=all` was neither an
  // option nor a class, and vanished without a word.
  private def parseBars(tokens: Seq[String]): Seq[String] = tokens
    .collect { case s"$key=$columns" if key.equalsIgnoreCase("bar") => columns.split(",").toSeq.map(_.trim).filter(_.nonEmpty) }
    .flatten

  private def csvPreferenceFrom(format: String): Option[CsvPreference] = format match {
    case "tsv" => Some(CsvPreference.TAB_PREFERENCE)
    case "csv" => Some(CsvPreference.STANDARD_PREFERENCE)
    case _ => None
  }

  private def normalizeCell(value: String): String = Option(value).getOrElse(defaultCellValue)

  /**
   * The number a cell holds, if it holds one. "1,234" is 1234, "-7%" is -7, "(1,234)" is -1234.
   * A string of digits too long for a Double reads as infinity, which the bar arithmetic would
   * turn into NaN percentages; it is not a number here.
   */
  def parseNumber(cell: String): Option[Double] = normalizeCell(cell).trim match {
    case regexNumber(sign, open, digits, close) if (open == null) == (close == null) =>
      Try(digits.replace(",", "").toDouble).toOption
        .filter(v => !v.isInfinite)
        .map(v => if (sign == "-" || open != null) -v else v)
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
      // Two finite values near the ends of a Double are infinitely far apart, and every bar on
      // that scale would be nothing.
      val span = hi - lo
      if (span > 0 && span < Double.PositiveInfinity) Some(BarScale(lo, hi)) else None
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
    // The header of a numeric column names tablesorter's parser. Left to guess, tablesorter reads
    // the first body cell whose text is not empty and sorts the column as text when that cell is
    // "-", "₩1,200" or "35명" -- 9 before 10, however many numbers follow. The digit parser it is
    // told to use strips everything but digits, sign, parentheses and separators, so it reads
    // every spelling parseNumber accepts. Header-column cells in body rows are never numeric
    // (numeric starts at thColumn), so one helper serves both kinds of <th>.
    def thOpen(columnIndex: Int): String =
      if (numeric.contains(columnIndex)) """<th class="sorter-digit">""" else "<th>"

    def tdOpen(columnIndex: Int, cell: String): String = {
      if (!numeric.contains(columnIndex)) return "<td>"
      parseNumber(cell) match {
        // A placeholder in a numeric column. tablesorter reads data-text before the cell's text,
        // an empty one included, so the empty attribute makes it skip the cell when it looks for
        // a parser and sort the cell last with the other empties, ascending or descending. Number
        // cells carry no data-text: the filter widget matches typed text against the same
        // attribute, and "65,438" has to find the cell that shows it.
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
      // is a header row, as written. A number names the column in that position; one that names
      // no position is tried as header text, so a column headed "2024" can still be named. A name
      // that is no column, or a column that is not numeric, draws nothing.
      val header: Seq[String] = if (shebang.thRow > 0) rows.headOption.getOrElse(Seq.empty).map(cell => normalizeCell(cell).trim) else Seq.empty
      def byHeader(ref: String): Seq[Int] = header.indices.filter(columnIndex => header(columnIndex).equalsIgnoreCase(ref))
      val named: Set[Int] = shebang.bars.flatMap {
        case "all" => numeric
        case ref if ref.forall(_.isDigit) =>
          ref.toIntOption.map(_ - 1).filter(columnIndex => columnIndex >= 0 && columnIndex < width).map(Seq(_)).getOrElse(byHeader(ref))
        case ref => byHeader(ref)
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
    case regexShebang(format, thRow, thColumn, trailing) =>
      // Split once; the classes and the options are read from the same words.
      val tokens: Seq[String] = Option(trailing).toSeq.flatMap(_.trim.split("""\s+""")).filter(_.nonEmpty)
      csvPreferenceFrom(format).map(csvPreference =>
        Shebang(csvPreference, thRow.toIntOrZero, thColumn.toIntOrZero, sanitizeClasses(tokens), parseBars(tokens))
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
            rowBuilder.append(columns.thOpen(columnIndex)).append(cachedCellToHtml(cell)).append("</th>")
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
