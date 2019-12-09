package com.aha00a.supercsv

import java.util

import org.supercsv.io.CsvListWriter
import scala.collection.JavaConversions._

object Implicits {
  implicit class RichCsvListWriter(csvListWriter: CsvListWriter) {
    def writeSeqSeqString(seqSeqString: Seq[Seq[String]]): Unit = {
      for (row <- seqSeqString) {
        val list: util.List[String] = row.toList
        csvListWriter.write(list)
      }
    }
  }
}
