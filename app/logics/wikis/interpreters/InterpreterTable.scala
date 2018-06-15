package logics.wikis.interpreters

import java.io.StringReader

import models.{WikiContext, PageContent}
import org.supercsv.io.CsvListReader
import org.supercsv.prefs.CsvPreference
import play.api.Logger
import com.aha00a.commons.utils.Using

import scala.collection.JavaConversions._
import scala.collection.mutable.ArrayBuffer
import scala.util.matching.Regex

object InterpreterTable {
  val regexShebang: Regex = """([ct]sv)(?:\s+(\d+)(?:\s+(\d+))?)?""".r

  case class Shebang(csvPreference:CsvPreference, thRow:Int, thColumn:Int)

  def interpret(pageContent: PageContent)(implicit wikiContext:WikiContext): String = {

    val shebang = parseShebang(pageContent.argument)
    shebang.map(shebang => {
      val arrayBuffer = ArrayBuffer[Array[String]]()
      Using(new CsvListReader(new StringReader(pageContent.content), shebang.csvPreference)) { listReader =>
        while (true) {
          val javaListString = listReader.read()
          if (null == javaListString) {
            return s"""<table class="simpleTable">""" +
              arrayBuffer
                .zipWithIndex
                .map(_.swap)
                .map(row => row._2.zipWithIndex.map(col => (row._1, col._2, col._1)))
                .map(_
                  .map(s =>
                    if (shebang.thRow <= s._1 && shebang.thColumn <= s._2)
                      s"<td>${s._3}</td>"
                    else
                      s"<th>${s._3}</th>"
                  ).mkString
                )
                .map(s => s"<tr>$s</tr>").mkString("\n") +
              "</table>"
          }
          val wiki: InterpreterWiki = new InterpreterWiki()
          arrayBuffer += javaListString.map(s => if(s == null) "" else s).map(s => {wiki.apply(s)}).toArray
        }
        Logger.error("")
        "Error"
      }
    }).getOrElse("Error!")
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
