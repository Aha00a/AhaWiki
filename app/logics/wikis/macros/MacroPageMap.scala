package logics.wikis.macros

import models.{AhaWikiQuery, WikiContext}

object MacroPageMap extends TraitMacro {
  override def apply(argument:String)(implicit wikiContext: WikiContext): String = { wikiContext.database.withConnection { implicit connection =>
    views.html.Wiki.graph(
      AhaWikiQuery().Link.selectAllButNotEmpty()
        .filter(l => l.and(wikiContext.setPageName.contains))
        .map(link => Array(link.src, link.dst))
        .toArray,
      enableWikiLink = true
    ).toString()
  }}
}
