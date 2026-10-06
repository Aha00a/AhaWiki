package logics.wikis

import com.aha00a.commons.utils.UuidUtil
import models.ContextWikiPage

import scala.collection.GenTraversableOnce
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object ExtractConvertInject {
  def markedBlocks(converter: String => String): ExtractConvertInject =
    new ExtractConvertInject {
      override def extract(s: String): String = extractByMarkers(s)
      override def convert(s: String)(implicit wikiContext: ContextWikiPage): String = converter(s)
    }
}

trait ExtractConvertInject {
  val arrayBuffer = new ArrayBuffer[(String, String)]()

  def getUniqueKey: String = {
    UuidUtil.newString
  }

  def extract(s: String): String

  protected final def extractByMarkers(s: String, open: String = "[[[", close: String = "]]]"): String = {
    if (s == null || !s.contains(open) || !s.contains(close)) {
      s
    } else {
      val Array(head, remain) = s.split(java.util.regex.Pattern.quote(open), 2)
      val Array(body, tail) = remain.split(java.util.regex.Pattern.quote(close), 2)
      val uniqueKey = getUniqueKey
      arrayBuffer += uniqueKey -> body
      head + uniqueKey + extractByMarkers(tail, open, close)
    }
  }

  def convert(s: String)(implicit wikiContext: ContextWikiPage): String

  def inject(s: String)(implicit wikiContext: ContextWikiPage): String =
    replaceKeys(s, arrayBuffer.map { case (key, value) => key -> convert(value) })

  /**
   * `s` with each key replaced by its text, building the result once.
   *
   * It used to be one `String.replace` per key, each copying the whole page so far: on a page with
   * many code spans and macros that was the single largest cost of a render (2026-10-07, 11% of
   * render time). Finding every key's places in `s` first and then building once gives the same
   * result. Replacing key by key never touched the text a key was replaced with -- a value can
   * only hold keys made before it, and those had already been replaced when it was inserted -- and
   * a key listed twice took its first text, which is what keeping the first one here does.
   *
   * `replacements` is evaluated whole, in order, before anything is replaced: converting a value
   * renders it, and renders have to happen in the order they always did.
   */
  protected final def replaceKeys(s: String, replacements: collection.Seq[(String, String)]): String = {
    val places = ArrayBuffer[(Int, String, String)]()
    val seen = mutable.Set[String]()
    for ((key, text) <- replacements if key.nonEmpty && seen.add(key)) {
      var i = s.indexOf(key)
      while (i >= 0) {
        places += ((i, key, text))
        i = s.indexOf(key, i + key.length)
      }
    }
    if (places.isEmpty) {
      s
    } else {
      val sb = new java.lang.StringBuilder(s.length)
      var from = 0
      for ((at, key, text) <- places.sortBy(_._1) if at >= from) {
        sb.append(s, from, at).append(text)
        from = at + key.length
      }
      sb.append(s, from, s.length).toString
    }
  }

  def contains(s:String): Boolean = arrayBuffer.exists(_._1 == s)
}
