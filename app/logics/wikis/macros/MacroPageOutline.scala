package logics.wikis.macros

import logics.wikis.WikiFormattable
import models.WikiContext

object MacroPageOutline extends WikiFormattable {
  override def apply(body: String)(implicit wikiContext: WikiContext): String = ""

  override def calcLength(body: String)(implicit wikiContext: WikiContext): Long = 0
}
