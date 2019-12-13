package com.aha00a.commons

import java.io.{File, PrintWriter}
import java.time.LocalDateTime

import com.aha00a.commons.utils.{DateTimeFormatterHolder, Using}

import scala.util.Random

object Implicits {
  implicit class LocalDateTimeFormatter(localDateTime:LocalDateTime) {
    def toIsoLocalDateTimeString: String = localDateTime.format(DateTimeFormatterHolder.isoLocalDateTime)
    def toIsoLocalDateString: String = localDateTime.format(DateTimeFormatterHolder.isoLocalDate)
    def toIsoLocalTimeString: String = localDateTime.format(DateTimeFormatterHolder.isoLocalTime)
    def toYearDashMonthString: String = localDateTime.format(DateTimeFormatterHolder.yearDashMonth)
  }

  implicit class RichFile(file: File) {
    def usingPrintWriter(f: java.io.File)(op: java.io.PrintWriter => Unit) {
      Using(new PrintWriter(f))(op)
    }

    def writeAll(s1: String): Unit = {
      usingPrintWriter(file)(_.write(s1))
    }

    def getSlashBasedPath: String = {
      if(File.separator != "/") {
        file.getPath.replaceAllLiterally(File.separator, "/")
      } else {
        file.getPath
      }
    }
  }

  implicit class RichSeq[T](seq:Seq[T]) {
    def getOrElse(i: Int, t:T): T = if(seq.isDefinedAt(i)) seq(i) else t
    def tailSafe(): Seq[T] = if(seq.length > 1) { seq.tail } else { Seq[T]() }
    def shuffle(): Seq[T] = Random.shuffle(seq)
    def groupByCount(): Map[T, Int] = seq.foldLeft(Map[T, Int]()) {
      (map, word) => map + (word -> (map.getOrElse(word, 0) + 1))
    }
    def random():T = seq(Random.nextInt(seq.size))
    def splitBy(by:(T, T) => Boolean): Iterator[Seq[T]] = {
      val cutIndice = seq.zipWithIndex.sliding(2).filter(s => by(s.head._1, s.last._1)).map(s => s.head._2).toSeq
      val ranges = -1 +: cutIndice :+ seq.length - 1
      ranges.sliding(2).map(i => seq.slice(i.head + 1, i.last + 1))
    }
  }

  implicit class RichTuple2IteratorT[T](t:(Iterator[T], Iterator[T])) {
    def concat(): Iterator[T] = t._1 ++ t._2
  }

  implicit class RichTuple2SeqT[T](t:(Seq[T], Seq[T])) {
    def concat(): Seq[T] = t._1 ++ t._2
  }

  implicit class RichString(s:String) {
    def isNullOrEmpty: Boolean = s == null || s.isEmpty
    def isNotNullOrEmpty: Boolean = !s.isNullOrEmpty
    def toOption: Option[String] = if (s.isNullOrEmpty) None else Some(s)
    def getOrElse(s:String): String = toOption.getOrElse(s)
    def escapeHtml(): String = s.replaceAll("""<""", "&lt;")
    def padLeft(len: Int, pad: String = " "): String = s.reverse.padTo(len, pad).reverse.mkString
    def padRight(len: Int, pad: String = " "): String = s.padTo(len, pad).mkString
    def toIntOrZero: Int = if(s == null || s == "") 0 else s.toInt
  }

  implicit def lambdaToRunnable(f: () => Unit): Runnable = new Runnable() { def run(): Unit = f() }
}
