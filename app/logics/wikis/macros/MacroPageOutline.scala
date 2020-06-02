package logics.wikis.macros

import models.WikiContext

object MacroPageOutline extends TraitMacro {
  override def toHtmlString(body: String)(implicit wikiContext: WikiContext): String = ""
}
