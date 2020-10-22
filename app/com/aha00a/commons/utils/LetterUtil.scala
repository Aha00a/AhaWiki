package com.aha00a.commons.utils

object LetterUtil {
  def firstLetterForIndex(s: String): Char = {
    import java.text.Normalizer

    val c = s.charAt(0)
    c match {
      case Hangul.regexHangul() =>
        Hangul(Hangul(c).index초성, 0, 0).c
      case _ =>
        Normalizer.normalize(c.toString, Normalizer.Form.NFD).replaceAll("\\p{InCombiningDiacriticalMarks}", "").charAt(0).toUpper
    }
  }
}
