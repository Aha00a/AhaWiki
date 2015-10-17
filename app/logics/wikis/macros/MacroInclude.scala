package logics.wikis.macros

import logics.wikis.Interpreters
import models.{MockDb, WikiContext}

object MacroInclude {
  def apply(argument: String)(implicit wikiContext: WikiContext): String = {
    MockDb.selectPageLastRevision(argument).map(w => Interpreters(w.content)).getOrElse("Error: " + argument)
  }
}
