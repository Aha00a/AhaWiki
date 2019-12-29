package logics.wikis.macros

import logics.wikis.interpreters.InterpreterWiki
import models.{AhaWikiQuery, WikiContext}

object MacroBacklinks extends TraitMacro {
  override def apply(argument:String)(implicit wikiContext: WikiContext): String = { wikiContext.database.withConnection { implicit connection =>
    InterpreterWiki(AhaWikiQuery().Link.select(wikiContext.name)
      .filterNot(_.or(_.contains("://")))
      .filter(_.dst == wikiContext.name)
      .map(l => s" * [${l.src}] ${l.alias}")
      .mkString("\n")
    )
  }}
}
