package logics.wikis

import models.WikiContext
import utils.{ShebangUtil, UuidUtil}

class WikiChunkProcessor() extends ExtractApply {

  def extract(s: String)(implicit wikiContext:WikiContext):String = {
    if (!s.contains("{{{") || !s.contains("}}}")) {
      s
    } else {
      val Array(head, remain) = s.split( """\{\{\{""", 2)
      val Array(body, tail) = remain.split( """\}\}\}""", 2)
      val uuid = UuidUtil.newString
      val result = Interpreters(ShebangUtil.addWhenNotExist(body, "text"))
      map.put(uuid, result)
      head + uuid + extract(tail)
    }
  }
}

