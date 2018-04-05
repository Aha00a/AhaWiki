package com.aha00a.commons.implicits

import java.io.{File, PrintWriter}
import java.time.LocalDateTime

import com.aha00a.commons.utils.{DateTimeFormatterHolder, Using}
import play.api.mvc.RequestHeader

import scala.util.Random

object Implicits {
  implicit class LocalDateTimeFormatter(localDateTime:LocalDateTime) {
    def toIsoLocalDateTimeString: String = localDateTime.format(DateTimeFormatterHolder.isoLocalDateTime)
    def toIsoLocalDateString: String = localDateTime.format(DateTimeFormatterHolder.isoLocalDate)
    def toIsoLocalTimeString: String = localDateTime.format(DateTimeFormatterHolder.isoLocalTime)
    def toYearDashMonthString: String = localDateTime.format(DateTimeFormatterHolder.yearDashMonth)
  }

  implicit class RichFile(cacheFileSh: File) {

    def usingPrintWriter(f: java.io.File)(op: java.io.PrintWriter => Unit) {
      Using(new PrintWriter(f))(op)
    }

    def writeAll(s1: String): Unit = {
      usingPrintWriter(cacheFileSh)(_.write(s1))
    }
  }

  implicit class RichSeq[T](seq:Seq[T]) {
    def tailSafe(): Seq[T] = if(seq.length > 1) { seq.tail } else { Seq[T]() }
    def shuffle(): Seq[T] = Random.shuffle(seq)
    def groupByCount(): Map[T, Int] = seq.foldLeft(Map[T, Int]()) {
      (map, word) => map + (word -> (map.getOrElse(word, 0) + 1))
    }
    def random():T = seq(Random.nextInt(seq.size))
  }

  implicit class RichString(s:String) {
    def toOption: Option[String] = if (s != null && s != "") Some(s) else None
    def getOrElse(s:String): String = toOption.getOrElse(s)
    def escapeHtml(): String = s.replaceAll("""<""", "&lt;")
    def padLeft(len: Int, pad: String = " "): String = s.reverse.padTo(len, pad).reverse.mkString
    def padRight(len: Int, pad: String = " "): String = s.padTo(len, pad).mkString
  }

  implicit class RichRequest(request:RequestHeader) {
    def isLocalhost:Boolean = request.headers.get("Host").getOrElse("").startsWith("localhost")
    def remoteAddressWithXRealIp: String = request.headers.get("X-Real-IP").getOrElse(request.remoteAddress)
    def header(key: String): Option[String] = request.headers.get(key)
    def userAgent: Option[String] = header("User-Agent")
    def referer: Option[String] = header("referer")
    def refererOrRoot: String =  referer.getOrElse("/")
  }

  implicit def lambdaToRunnable(f: () => Unit): Runnable = new Runnable() { def run(): Unit = f() }
}
