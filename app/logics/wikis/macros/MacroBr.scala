package logics.wikis.macros

import models.WikiContext

object MacroBr extends WikiFormattable {
  override def apply(body: String)(implicit wikiContext: WikiContext): String = "<br/>"

  override def calcLength(body: String)(implicit wikiContext: WikiContext): Long = 1
}
