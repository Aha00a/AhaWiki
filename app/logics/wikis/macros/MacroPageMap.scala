package logics.wikis.macros

import models.ContextWikiPage

object MacroPageMap extends TraitMacro {
  override def toHtmlString(argument:String)(implicit wikiContext: ContextWikiPage): String = { wikiContext.database.withConnection { implicit connection =>
    import models.tables.Link
    import models.tables.Site
    implicit val site: Site = wikiContext.site
    views.html.Wiki.graph(
      Link.selectAllButNotEmpty()
        .filter(l => l.and(wikiContext.pageCanSee))
        .map(link => Array(link.src, link.dst))
        .toArray,
      enableWikiLink = true
    ).toString()
  }}
}
