package com.aha00a.commons.utils

object LetterUtil {
  def firstLetterForIndex(s: String): Char = {
    import java.text.Normalizer

    import scala.util.matching.Regex

    val c = s.charAt(0)
    val regexHangul: Regex = """[가-힣]""".r
    c match {
      case regexHangul() =>
        val index = (c - 0xAC00) / (21 * 28)
        (0xAC00 + index * 588).toChar
      case _ =>
        Normalizer.normalize(c.toString, Normalizer.Form.NFD).replaceAll("\\p{InCombiningDiacriticalMarks}", "").charAt(0).toUpper
    }
  }
}
