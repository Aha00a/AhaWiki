package logics.wikis.macros

import logics.wikis.interpreters.InterpreterWiki
import models.{DirectQuery, WikiContext}

object MacroBacklinks {
  def apply()(implicit wikiContext: WikiContext) = {
    new InterpreterWiki().interpret(DirectQuery.linkSelect(wikiContext.name)
      .filterNot(_.or(_.contains("://")))
      .filter(_.dst == wikiContext.name)
      .map(l => s" * [${l.src}]")
      .mkString("\n")
    )
  }
}
