package logics.wikis

import com.aha00a.commons.utils.UuidUtil
import models.ContextWikiPage

import scala.collection.GenTraversableOnce
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

  def inject(s: String)(implicit wikiContext: ContextWikiPage): String = {
    var result = s
    for ((key, value) <- arrayBuffer) {
      result = result.replace(key, convert(value))
    }
    result
  }

  def contains(s:String): Boolean = arrayBuffer.exists(_._1 == s)
}
