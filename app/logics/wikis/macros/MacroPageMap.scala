package logics.wikis.macros

import models.{Database, WikiContext}

object MacroPageMap extends TraitMacro {
  override def apply(argument:String)(implicit wikiContext: WikiContext) = {
    views.html.Wiki.graph(
      Database.linkSelect()
        .filterNot(l => l.src.contains("://"))
        .filterNot(l => l.dst.contains("://"))
        .map(link => Array(link.src, link.dst))
        .toArray,
      true
    ).toString()
  }
}
