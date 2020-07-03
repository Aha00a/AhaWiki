package logics.wikis.macros

import logics.wikis.interpreters.InterpreterWiki
import models.{AhaWikiQuery, WikiContext}

object MacroBacklinks extends TraitMacro {
  override def toHtmlString(argument:String)(implicit wikiContext: WikiContext): String = { wikiContext.database.withConnection { implicit connection =>
    import models.tables.Link
    val listLink: List[Link] = Link.selectDst(wikiContext.name)
    val listLinkFiltered = listLink.filter(l => l.and(wikiContext.pageCanSee))
    InterpreterWiki.toHtmlString(listLinkFiltered
      .map(l => s""" * ["${l.src}"] ${l.alias}""")
      .mkString("\n")
    )
  }}
}
