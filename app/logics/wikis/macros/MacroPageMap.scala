package logics.wikis.macros

import implicits.Implicits._
import models.{WikiContext, DirectQuery}

object MacroPageMap {
  def apply()(implicit wikiContext: WikiContext) = {
    views.html.Wiki.graph(
      DirectQuery.linkSelect()
        .filterNot(l => l.src.contains("://"))
        .filterNot(l => l.dst.contains("://"))
        .shuffle()
        .take(100)
        .map(link => Array(link.src, link.dst))
        .toArray,
      true
    ).toString()
  }
}
