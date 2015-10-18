package logics.wikis

import models.WikiContext
import utils.ShebangUtil

class ExtractConvertApplyChunk() extends ExtractConvertApply {

  def extract(s: String):String = {
    if (!s.contains("{{{") || !s.contains("}}}")) {
      s
    } else {
      val Array(head, remain) = s.split( """\{\{\{""", 2)
      val Array(body, tail) = remain.split( """\}\}\}""", 2)
      val uniqueKey = getUniqueKey
      map.put(uniqueKey, body)
      head + uniqueKey + extract(tail)
    }
  }

  def convert(s:String)(implicit wikiContext:WikiContext):String = Interpreters(ShebangUtil.addWhenNotExist(s, "text"))
}

