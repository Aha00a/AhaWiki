package logics.wikis

import models.WikiContext
import utils.{ShebangUtil, UuidUtil}

class WikiChunkProcessor() extends ExtractApply {

  def extract(s: String):String = {
    if (!s.contains("{{{") || !s.contains("}}}")) {
      s
    } else {
      val Array(head, remain) = s.split( """\{\{\{""", 2)
      val Array(body, tail) = remain.split( """\}\}\}""", 2)
      val uuid = UuidUtil.newString
      map.put(uuid, body)
      head + uuid + extract(tail)
    }
  }

  def convert(s:String)(implicit wikiContext:WikiContext):String = Interpreters(ShebangUtil.addWhenNotExist(s, "text"))
}

