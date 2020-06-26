package com.aha00a.supercsv

import java.util

import org.supercsv.io.CsvListWriter
import scala.collection.JavaConverters._

object Implicits {
  implicit class RichCsvListWriter(csvListWriter: CsvListWriter) {
    def writeSeqSeqString(seqSeqString: Seq[Seq[String]]): Unit = {
      for (row <- seqSeqString) {
        val list: util.List[String] = row.toList.asJava
        csvListWriter.write(list)
      }
    }
  }
}
