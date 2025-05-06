package logics.wikis.macros

import logics.DefaultPageLogic
import logics.wikis.interpreters.InterpreterWiki
import models.ContextWikiPage
import models.tables.Link
import models.tables.Site

object MacroYears extends TraitMacro {
  override def toHtmlString(argument:String)(implicit wikiContext: ContextWikiPage): String = { wikiContext.database.withConnection { implicit connection =>
    implicit val site: Site = wikiContext.site

    InterpreterWiki.replaceLink(
      Link.selectDistinctDstWhereDstIsYear()
        .filter(v => wikiContext.setPageNameByPermission.contains(v) || DefaultPageLogic.isDefined(v))
        .map(name => s"[$name]")
        .mkString(", ")
    )
  }}
}
