package logics.wikis

import com.aha00a.commons.utils.UuidUtil
import models.WikiContext

import scala.collection.mutable.ArrayBuffer

trait ExtractConvertApply {
  val arrayBuffer = new ArrayBuffer[(String, String)]()

  def getUniqueKey: String = {
    UuidUtil.newString
  }

  def extract(s: String): String

  def convert(s: String)(implicit wikiContext: WikiContext): String

  def apply(s: String)(implicit wikiContext: WikiContext): String = {
    var result = s
    for ((key, value) <- arrayBuffer) {
      result = result.replace(key, convert(value))
    }
    result
  }

  def contains(s:String): Boolean = arrayBuffer.exists(_._1 == s)
}

