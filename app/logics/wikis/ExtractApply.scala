package logics.wikis

import models.WikiContext

import scala.collection.mutable

trait ExtractApply {
  val map = new mutable.HashMap[String, String]()
  def extract(s: String)(implicit wikiContext:WikiContext):String

  def apply(s:String):String = {
    var result = s
    for((key, value) <- map) {
      result = result.replace(key, value)
    }
    result
  }
}

