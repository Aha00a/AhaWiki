package logics.wikis.macros

import logics.wikis.interpreters.InterpreterWiki
import models.{Database, WikiContext}

object MacroBacklinks extends TraitMacro {
  override def apply(argument:String)(implicit wikiContext: WikiContext) = {
    new InterpreterWiki().apply(Database().linkSelect(wikiContext.name)
      .filterNot(_.or(_.contains("://")))
      .filter(_.dst == wikiContext.name)
      .map(l => s" * [${l.src}]")
      .mkString("\n")
    )
  }
}
