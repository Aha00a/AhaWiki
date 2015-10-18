package implicits

import java.io.{PrintWriter, File}
import java.time.LocalDateTime

import utils.{Using, DateTimeFormatterHolder}

import scala.util.Random

object Implicits {
  implicit class LocalDateTimeFormatter(localDateTime:LocalDateTime) {
    def toIsoDateTimeString = localDateTime.format(DateTimeFormatterHolder.isoDateTime)
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
  }

  implicit class RichString(s:String) {
    def escapeHtml(): String = s.replaceAll("""<""", "&lt;")
  }
}
