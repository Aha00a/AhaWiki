package com.aha00a.supercsv

import java.util

import org.supercsv.io.CsvListWriter

import scala.jdk.CollectionConverters._

object Implicits {
  implicit class RichCsvListWriter(csvListWriter: CsvListWriter) {
    def writeSeqSeqString(seqSeqString: Seq[Seq[String]]): Unit = {
      for (row <- seqSeqString) {
        csvListWriter.write(row.toList.asJava)
      }
    }
  }
}
