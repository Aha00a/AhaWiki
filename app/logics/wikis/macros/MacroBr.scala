package logics.wikis.macros

import models.WikiContext

object MacroBr extends TraitMacro {
  override def apply(argument: String)(implicit wikiContext: WikiContext): String = "<br/>"

  override def calcLength(argument: String)(implicit wikiContext: WikiContext): Long = 1
}
