package logics.wikis

import models.WikiContext

import scala.collection.mutable

trait ExtractConvertApply {
  val map = new mutable.HashMap[String, String]()

  def extract(s: String): String

  def convert(s: String)(implicit wikiContext: WikiContext): String

  def apply(s: String)(implicit wikiContext: WikiContext): String = {
    var result = s
    for ((key, value) <- map) {
      result = result.replace(key, convert(value))
    }
    result
  }
}

