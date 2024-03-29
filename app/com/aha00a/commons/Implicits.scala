package com.aha00a.commons

import java.io.File
import java.io.PrintWriter
import java.time.LocalDate
import java.time.LocalDateTime
import java.util.Date

import com.aha00a.commons.utils.DateTimeFormatterHolder
import com.aha00a.commons.utils.LocalDateTimeUtil
import com.aha00a.commons.utils.Using

import scala.util.Random
import scala.util.Try

object Implicits {
  implicit class RichBoolean(b:Boolean) {
    def to01: String = if(b) "1" else "0"
    def toYN: String = if(b) "Y" else "N"
  }

  implicit class RichString(s:String) {
    def isNullOrEmpty: Boolean = s == null || s.isEmpty
    def isNotNullOrEmpty: Boolean = !s.isNullOrEmpty
    def toOption: Option[String] = if (s.isNullOrEmpty) None else Some(s)
    def getOrElse(rhs:String): String = toOption.getOrElse(rhs)

    def containsIgnoreCase(rhs: String): Boolean = s.toLowerCase().contains(rhs.toLowerCase())

    def escapeHtml(): String = s.replaceAll("""<""", "&lt;")

    def padLeft(len: Int, pad: String = " "): String = s.reverse.padTo(len, pad).reverse.mkString
    def padRight(len: Int, pad: String = " "): String = s.padTo(len, pad).mkString

    def toIntOrZero: Int = Try(s.toInt).toOption.getOrElse(0)
    def toDoubleOrZero: Double = Try(s.toDouble).toOption.getOrElse(0)
    def toBoolGenerously: Boolean = {
      import scala.util.matching.Regex
      if(s.isNullOrEmpty)
        return false

      val regexTrueForOneOfThese: Regex = """^[1tTyY]$""".r
      s match {
        case regexTrueForOneOfThese() => true
        case "true" | "True" | "TRUE" => true
        case _ => false
      }
    }

    def splitLines(): Array[String] = s.split("""(\r\n|\n)""")
    def splitTabs(): Array[String] = s.split("""\t""")
    def splitCommaIgnoreAroundWhitespace(): Array[String] = s.split("""\s*,\s*""")
    def splitLinesSeq(): Seq[String] = s.splitLines().toSeq
    def splitTabsSeq(): Seq[String] = s.splitTabs().toSeq
  }

  implicit class RichDate(date:Date) {
    def toLocalDateTime: LocalDateTime = LocalDateTimeUtil.convert(date)
  }



  implicit class LocalDateTimeFormatter(localDateTime:LocalDateTime) {
    def toIsoLocalDateTimeString: String = localDateTime.format(DateTimeFormatterHolder.isoLocalDateTime)
    def toIsoLocalDateString: String = localDateTime.format(DateTimeFormatterHolder.isoLocalDate)
    def toIsoLocalTimeString: String = localDateTime.format(DateTimeFormatterHolder.isoLocalTime)
    def toYearDashMonthString: String = localDateTime.format(DateTimeFormatterHolder.yearDashMonth)
  }

  implicit class LocalDateFormatter(localDate:LocalDate) {
    def toIsoLocalDateString: String = localDate.format(DateTimeFormatterHolder.isoLocalDate)

    def toYearDashMonthString: String = localDate.format(DateTimeFormatterHolder.yearDashMonth)
    def toYearString: String = localDate.format(DateTimeFormatterHolder.year)
    def toMonthDashDayString: String = localDate.format(DateTimeFormatterHolder.monthDashDay)
    def toDashDashMonthDashDayString: String = localDate.format(DateTimeFormatterHolder.dashDashMonthDashDay)

  }


  implicit class RichSeq[T](seq:Seq[T]) {
    def getOrElse(i: Int, t:T): T = if(seq.isDefinedAt(i)) seq(i) else t
    def getWithMinMax(i: Int): T = seq(Math.max(Math.min(i, seq.length - 1), 0))
    def tailSafe(): Seq[T] = if(seq.length > 1) { seq.tail } else { Seq[T]() }
    def shuffle(): Seq[T] = Random.shuffle(seq)
    def groupByCount(): Map[T, Int] = seq.foldLeft(Map[T, Int]()) {
      (map, word) => map + (word -> (map.getOrElse(word, 0) + 1))
    }
    def random():T = seq(Random.nextInt(seq.size))
    def splitBy(by:(T, T) => Boolean): Iterator[Seq[T]] = {
      val cutIndices = seq.zipWithIndex.sliding(2).filter(s => by(s.head._1, s.last._1)).map(s => s.head._2).toSeq
      val ranges = -1 +: cutIndices :+ seq.length - 1
      ranges.sliding(2).map(i => seq.slice(i.head + 1, i.last + 1))
    }
  }

  implicit class RichTuple2IteratorT[T](t:(Iterator[T], Iterator[T])) {
    def concat(): Iterator[T] = t._1 ++ t._2
  }

  implicit class RichTuple2SeqT[T](t:(Seq[T], Seq[T])) {
    def concat(): Seq[T] = t._1 ++ t._2
  }



  implicit def lambdaToRunnable(f: () => Unit): Runnable = () => f()


  implicit class RichFile(file: File) {
    def usingPrintWriter(f: java.io.File)(op: java.io.PrintWriter => Unit): Unit = {
      import java.io.FileOutputStream
      import java.io.OutputStreamWriter
      Using(new PrintWriter(new OutputStreamWriter(new FileOutputStream(f), "UTF-8")))(op)
    }

    def readAllString(): String = {
      import scala.io.Codec
      implicit val codec: Codec = Codec.UTF8
      Using(scala.io.Source.fromFile(file))(_.mkString)
    }

    def writeAll(s1: String): Unit = {
      usingPrintWriter(file)(_.write(s1))
    }

    def getSlashBasedPath: String = {
      if(File.separator != "/") {
        file.getPath.replace(File.separator, "/")
      } else {
        file.getPath
      }
    }
  }
}
