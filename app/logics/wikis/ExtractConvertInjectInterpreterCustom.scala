package logics.wikis

import models.ContextWikiPage

// TODO: Refactor this.
class ExtractConvertInjectInterpreterCustom(converter:String => String) extends ExtractConvertInject {
  override def extract(s: String): String = {
    extractByMarkers(s)
  }

  override def convert(s: String)(implicit wikiContext: ContextWikiPage): String = converter(s)
}
