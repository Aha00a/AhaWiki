package logics.wikis.macros

import logics.wikis.PageLogic
import logics.wikis.interpreters.InterpreterWiki
import models.{AhaWikiQuery, Link, WikiContext}

object MacroBacklinks extends TraitMacro {
  override def apply(argument:String)(implicit wikiContext: WikiContext): String = { wikiContext.database.withConnection { implicit connection =>
    val listLink: List[Link] = AhaWikiQuery().Link.selectDst(wikiContext.name)
    val listLinkFiltered = listLink.filter(l => wikiContext.setPageName.contains(l.src))
    InterpreterWiki(listLinkFiltered
      .map(l => s""" * ["${l.src}"] ${l.alias}""")
      .mkString("\n")
    )
  }}
}
