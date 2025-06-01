package logics.wikis.macros

import logics.DefaultPageLogic
import logics.wikis.interpreters.InterpreterWiki
import models.ContextWikiPage
import models.tables.CalculatedLink
import models.tables.Site

object MacroYears extends TraitMacro {
  override def toHtmlString(argument:String)(implicit wikiContext: ContextWikiPage): String = { wikiContext.database.withConnection { implicit connection =>
    implicit val site: Site = wikiContext.site

    InterpreterWiki.replaceLink(
      CalculatedLink.selectDistinctDstWhereDstIsYear()
        .filter(v => wikiContext.setPageNameByPermission.contains(v) || DefaultPageLogic.isDefined(v))
        .map(name => s"[$name]")
        .mkString(", ")
    )
  }}
}
