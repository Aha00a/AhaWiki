package logics.wikis.macros

import models.{AhaWikiDatabase, WikiContext}

object MacroPageMap extends TraitMacro {
  override def apply(argument:String)(implicit wikiContext: WikiContext) = {
    views.html.Wiki.graph(
      AhaWikiDatabase().linkSelect()
        .filterNot(l => l.src.contains("://"))
        .filterNot(l => l.dst.contains("://"))
        .map(link => Array(link.src, link.dst))
        .toArray,
      true
    ).toString()
  }
}
