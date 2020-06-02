package logics.wikis.macros

import models.WikiContext

object MacroBr extends TraitMacro {
  override def toHtmlString(argument: String)(implicit wikiContext: WikiContext): String = "<br/>"
}
