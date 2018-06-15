package logics.wikis.interpreters

import java.io.StringReader
import java.util

import com.aha00a.commons.utils.Using
import models.{PageContent, WikiContext}
import org.supercsv.io.CsvListReader
import org.supercsv.prefs.CsvPreference

import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.matching.Regex

object InterpreterTable {
  val regexShebang: Regex = """([ct]sv)(?:\s+(\d+)(?:\s+(\d+))?)?(?:\s+([a-zA-Z]\w*))?""".r

  case class Shebang(csvPreference:CsvPreference, thRow:Int, thColumn:Int)

  def interpret(pageContent: PageContent)(implicit wikiContext:WikiContext): String = {
    val shebang = parseShebang(pageContent.argument)
    shebang.map(shebang => {
      val arrayBuffer = ArrayBuffer[Array[String]]()
      Using(new CsvListReader(new StringReader(pageContent.content), shebang.csvPreference)) { listReader =>
        val rowColumnData = convert(listReader)
          .map(row => row
            .map(s => if(s == null) "" else new InterpreterWiki().apply(s))
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
        s"""<table class="simpleTable"><thead>$thead</thead><tbody>$tbody</tbody></table>"""
      }
    }).getOrElse("Error!")
  }

  def convert(reader: CsvListReader): mutable.Seq[util.List[String]] = {
    val arrayBuffer = ArrayBuffer[util.List[String]]()
    while (true) {
      val javaListString = reader.read()
      if (null == javaListString)
        return arrayBuffer
      
      arrayBuffer += javaListString
    }
    throw new Exception()
  }

  def parseShebang(argument:Seq[String]): Option[Shebang] = argument match {
    case Seq("tsv") => Some(Shebang(CsvPreference.TAB_PREFERENCE, 0, 0))
    case Seq("tsv", i) => Some(Shebang(CsvPreference.TAB_PREFERENCE, i.toInt, 0))
    case Seq("tsv", i, j) => Some(Shebang(CsvPreference.TAB_PREFERENCE, i.toInt, j.toInt))
    case Seq("csv") => Some(Shebang(CsvPreference.STANDARD_PREFERENCE, 0, 0))
    case Seq("csv", i) => Some(Shebang(CsvPreference.STANDARD_PREFERENCE, i.toInt, 0))
    case Seq("csv", i, j) => Some(Shebang(CsvPreference.STANDARD_PREFERENCE, i.toInt, j.toInt))
    case _ => None
  }
}
