package com.aha00a.supercsv

import java.io.StringWriter

import com.aha00a.commons.utils.Using
import com.aha00a.supercsv.Implicits._
import org.supercsv.io.CsvListWriter
import org.supercsv.prefs.CsvPreference

object SupercsvUtil {
  def toTsvString(seqSeqString: Seq[Seq[String]]): String = {
    toString(seqSeqString, CsvPreference.TAB_PREFERENCE)
  }

  def toString(seqSeqString: Seq[Seq[String]], csvPreference: CsvPreference): String = {
    Using(new StringWriter()) { stringWriter =>
      Using(new CsvListWriter(stringWriter, csvPreference)) { csvListWriter =>
        csvListWriter.write(seqSeqString)
      }
      stringWriter.toString
    }
  }
}
