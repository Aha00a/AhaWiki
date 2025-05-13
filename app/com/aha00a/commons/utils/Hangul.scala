package com.aha00a.commons.utils

case class Hangul(c: Char, index초성: Int, index중성: Int, index종성: Int)

object Hangul {

  import scala.util.matching.Regex

  val start: Char = '가'
  val end: Char = '힣'

  val regexHangul: Regex = """[가-힣]""".r

  //noinspection ScalaUnusedSymbol
  val count초성 = 19
  val count중성 = 21
  val count종성 = 28

  def apply(c: Char): Hangul = {
    val index초성 = (c - start) / (count중성 * count종성)
    val index중성 = (c - start) % (count중성 * count종성) / count종성
    val index종성 = (c - start) % count종성
    Hangul(c, index초성, index중성, index종성)
  }

  def apply(index초성: Int, index중성: Int, index종성: Int): Hangul = {
    val c = (start + (index초성 * count중성 * count종성) + (index중성 * count종성) + index종성).toChar
    Hangul(c, index초성, index중성, index종성)
  }

  //noinspection SimplifyBoolean
   def containsKo(v: String): Boolean = v.exists(c =>
    (0xAC00 <= c && c <= 0xD7A3) || // Hangul Syllables (한글 음절)
    (0x3130 <= c && c <= 0x318F) || // Hangul Compatibility Jamo (한글 호환 자모)
    (0xA960 <= c && c <= 0xA97C) || // Hangul Jamo Extended-A (한글 자모 확장-A)
    (0xD7B0 <= c && c <= 0xD7C6) || // Hangul Jamo Extended-B (한글 자모 확장-B)
    (0x1100 <= c && c <= 0x115F) || // Hangul Jamo (한글 자모)
    false
  )
}
