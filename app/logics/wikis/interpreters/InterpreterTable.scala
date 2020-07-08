package logics.wikis.interpreters

import java.io.StringReader

import com.aha00a.commons.Implicits._
import com.aha00a.commons.utils.Using
import models.{PageContent, WikiContext}
import org.supercsv.io.CsvListReader
import org.supercsv.prefs.CsvPreference

import scala.jdk.CollectionConverters._
import scala.collection.mutable.ArrayBuffer
import scala.util.matching.Regex

object InterpreterTable extends TraitInterpreter {

  import models.tables.Link

  val regexShebang: Regex = """([ct]sv)(?:\s+(\d+)(?:\s+(\d+))?)?(?:\s+(.+))?""".r

  case class Shebang(csvPreference:CsvPreference, thRow:Int, thColumn:Int, classes:String) {
    def getClasses: String = classes.toOption match {
      case Some(s) => s"simpleTable $s"
      case None => "simpleTable"
    }
  }

  def parseShebang(argument:Seq[String]): Option[Shebang] = argument.mkString(" ") match {
    case regexShebang("tsv", thRow, thColumn, classes) => Some(Shebang(CsvPreference.TAB_PREFERENCE, thRow.toIntOrZero, thColumn.toIntOrZero, classes))
    case regexShebang("csv", thRow, thColumn, classes) => Some(Shebang(CsvPreference.STANDARD_PREFERENCE, thRow.toIntOrZero, thColumn.toIntOrZero, classes))
    case _ => None
  }

  override def toHtmlString(content: String)(implicit wikiContext:WikiContext): String = {
    val pageContent: PageContent = PageContent(content)
    val shebang = parseShebang(pageContent.argument)
    shebang.map(shebang => {
      Using(new CsvListReader(new StringReader(pageContent.content), shebang.csvPreference)) { listReader =>
        val rowColumnData = convert(listReader)
          .map(row => row
            .map(s => if(s == null) "" else InterpreterWiki.toHtmlString(s))
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
      }
    }).getOrElse("Error!")
  }

  def convert(reader: CsvListReader): Seq[Seq[String]] = {
    val arrayBuffer = ArrayBuffer[Seq[String]]()
    while (true) {
      val javaListString = reader.read().asScala
      if (null == javaListString)
        return arrayBuffer.toSeq
      
      arrayBuffer += javaListString.toSeq
    }
    throw new Exception()
  }

  override def toSeqLink(content: String)(implicit wikiContext: WikiContext): Seq[Link] = {
    // TODO
    Seq()
  }
}
