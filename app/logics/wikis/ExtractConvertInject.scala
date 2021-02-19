package logics.wikis

import com.aha00a.commons.utils.UuidUtil
import models.ContextWikiPage

import scala.collection.GenTraversableOnce
import scala.collection.mutable.ArrayBuffer

trait ExtractConvertInject {
  val arrayBuffer = new ArrayBuffer[(String, String)]()

  def getUniqueKey: String = {
    UuidUtil.newString
  }

  def extract(s: String): String

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

