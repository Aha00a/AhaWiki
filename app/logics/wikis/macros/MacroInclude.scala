package logics.wikis.macros

import logics.wikis.WikiPermission
import logics.wikis.interpreters.Interpreters
import models.PageContent
import models.WikiContext

object MacroInclude extends TraitMacro {
  override def toHtmlString(argument: String)(implicit wikiContext: WikiContext): String = doApply(argument, s => s)
  def doApply(argument: String, preprocessor:String => String)(implicit wikiContext: WikiContext): String = { wikiContext.database.withConnection { implicit connection =>
    import models.WikiContext.Provider
    import models.tables.Site
    import play.api.cache.SyncCacheApi
    import play.api.db.Database
    implicit val provider: Provider = wikiContext.provider
    implicit val database: Database = wikiContext.database
    implicit val site: Site = wikiContext.site
    val pageLastRevision = models.tables.Page.selectLastRevision(argument)
    if (WikiPermission().isReadable(pageLastRevision.map(s => PageContent(s.content)))) {
      pageLastRevision.map(w => Interpreters.toHtmlString(preprocessor(w.content))).getOrElse("Error: " + argument)
    } else {
      MacroError.toHtmlString(s"Permission Denied - [[$name($argument)]]")
    }
  }}

  override def extractLink(argument: String)(implicit wikiContext: WikiContext): Seq[String] = Seq(argument)
}
