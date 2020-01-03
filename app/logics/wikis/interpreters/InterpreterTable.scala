package logics.wikis.interpreters

import java.io.StringReader

import com.aha00a.commons.Implicits._
import com.aha00a.commons.utils.Using
import models.{PageContent, WikiContext}
import org.supercsv.io.CsvListReader
import org.supercsv.prefs.CsvPreference

import scala.collection.JavaConversions._
import scala.collection.mutable.ArrayBuffer
import scala.util.matching.Regex

object InterpreterTable {
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

  def interpret(pageContent: PageContent)(implicit wikiContext:WikiContext): String = {
    val shebang = parseShebang(pageContent.argument)
    shebang.map(shebang => {
      Using(new CsvListReader(new StringReader(pageContent.content), shebang.csvPreference)) { listReader =>
        val rowColumnData = convert(listReader)
          .map(row => row
            .map(s => if(s == null) "" else InterpreterWiki(s))
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
          s"""<table class="macroTable ${shebang.getClasses}"><tbody>$tbody</tbody></table>"""
        else
          s"""<table class="macroTable ${shebang.getClasses}"><thead>$thead</thead><tbody>$tbody</tbody></table>"""
      }
    }).getOrElse("Error!")
  }

  def convert(reader: CsvListReader): Seq[Seq[String]] = {
    val arrayBuffer = ArrayBuffer[Seq[String]]()
    while (true) {
      val javaListString = reader.read()
      if (null == javaListString)
        return arrayBuffer
      
      arrayBuffer += javaListString.toSeq
    }
    throw new Exception()
  }

}
