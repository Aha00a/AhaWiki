package logics.wikis.macros

import logics.wikis.WikiPermission
import logics.wikis.interpreters.Interpreters
import models.PageContent
import models.WikiContext

object MacroIncludeStartsWith extends TraitMacro {                 // TODO: design & implement
  @scala.annotation.tailrec
  override def toHtmlString(argument: String)(implicit wikiContext: WikiContext): String = argument match {
    case "" | null => toHtmlString(wikiContext.name)
    case _ => wikiContext.database.withConnection { implicit connection =>
      import models.WikiContext.Provider
      import models.tables.PageWithoutContentWithSize
      import models.tables.Site
      import play.api.cache.SyncCacheApi
      import play.api.db.Database
      implicit val provider: Provider = wikiContext.provider
      implicit val syncCacheApi: SyncCacheApi = wikiContext.syncCacheApi
      implicit val database: Database = wikiContext.database
      implicit val site: Site = wikiContext.site

      val list: List[PageWithoutContentWithSize] = wikiContext.listPageByPermission
      list.filter(p => p.name != argument && p.name.startsWith(argument)).map(page => {
        val pageLastRevision = models.tables.Page.selectLastRevision(page.name)
        if (WikiPermission().isReadable(pageLastRevision.map(s => PageContent(s.content)))) {
          pageLastRevision.map(w => Interpreters.toHtmlString(w.content.replaceFirst("""^= .+""", s"== [${w.name}]"))).getOrElse("Error: " + argument)
        } else {
          MacroError.toHtmlString(s"Permission Denied - [[$name($argument)]]")
        }
      }).mkString("\n")
    }
  }

  override def extractLink(argument: String)(implicit wikiContext: WikiContext): Seq[String] = Seq(argument)
}
