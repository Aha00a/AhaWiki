package logics.wikis.macros

import models.{AhaWikiQuery, WikiContext}

object MacroPageMap extends TraitMacro {
  override def apply(argument:String)(implicit wikiContext: WikiContext): String = { wikiContext.database.withConnection { implicit connection =>
    views.html.Wiki.graph(
      AhaWikiQuery().Link.linkSelectNotUrl()
        .filterNot(l => l.src.contains("://"))
        .filterNot(l => l.dst.contains("://"))
        .map(link => Array(link.src, link.dst))
        .toArray,
      enableWikiLink = true
    ).toString()
  }}
}
