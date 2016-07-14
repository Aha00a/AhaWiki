package logics.wikis.macros

import models.WikiContext

object MacroPageOutline extends TraitMacro {
  override def apply(body: String)(implicit wikiContext: WikiContext): String = ""

  override def calcLength(body: String)(implicit wikiContext: WikiContext): Long = 0
}
