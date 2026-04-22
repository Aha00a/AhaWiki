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
      val rowColumnData = rows
        .map(row => row
          .map(normalizeCell)
          .map(InterpreterWiki.toHtmlString)
          .zipWithIndex
        )
        .zipWithIndex
      val (head, body) = rowColumnData.partition(r => r._2 < shebang.thRow)
      val thead = head
        .map(_._1
          .map(col => s"<th>${col._1}</th>")
          .mkString
        )
        .map(s => s"<tr>$s</tr>")
        .mkString("\n")
      val tbody = body
        .map(_._1
          .map(col => if (col._2 < shebang.thColumn) s"<th>${col._1}</th>" else s"<td>${col._1}</td>")
          .mkString
        )
        .map(s => s"<tr>$s</tr>").mkString("\n")
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
      rows.flatMap(_.map(normalizeCell).flatMap(InterpreterWiki.toSeqLink))
    }.getOrElse(Seq())
  }
}
