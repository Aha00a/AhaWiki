package logics.wikis.macros

import logics.wikis.interpreters.InterpreterWiki
import models.{AhaWikiDatabase, WikiContext}

object MacroBacklinks extends TraitMacro {
  override def apply(argument:String)(implicit wikiContext: WikiContext): String = {
    new InterpreterWiki().apply(AhaWikiDatabase()(wikiContext.database).LinkTable.linkSelectNotUrl(wikiContext.name)
      .filterNot(_.or(_.contains("://")))
      .filter(_.dst == wikiContext.name)
      .map(l => s" * [${l.src}]")
      .mkString("\n")
    )
  }
}
