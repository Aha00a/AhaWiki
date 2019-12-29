package logics.wikis.macros

import models.WikiContext

object MacroBr extends TraitMacro {
  override def apply(argument: String)(implicit wikiContext: WikiContext): String = "<br/>"
}
