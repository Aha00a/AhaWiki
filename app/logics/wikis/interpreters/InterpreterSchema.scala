package logics.wikis.interpreters

import logics.wikis.macros.MacroError
import models.{PageContent, WikiContext}

object InterpreterSchema {
  def apply(pageContent: PageContent)(implicit wikiContext: WikiContext): String = {
    MacroError("Not Implemented, yet")
  }
}
