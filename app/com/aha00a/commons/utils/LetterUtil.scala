package com.aha00a.commons.utils

object LetterUtil {
  def firstLetterForIndex(s: String): Char = {
    import java.text.Normalizer

    import scala.util.matching.Regex

    val c = s.charAt(0)
    val regexHangul: Regex = """[가-힣]""".r
    c match {
      case regexHangul() =>
        Hangul(Hangul(c).index초성, 0, 0).c
      case _ =>
        Normalizer.normalize(c.toString, Normalizer.Form.NFD).replaceAll("\\p{InCombiningDiacriticalMarks}", "").charAt(0).toUpper
    }
  }
}
