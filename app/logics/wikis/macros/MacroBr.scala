package logics.wikis.macros

import logics.wikis.WikiFormattable
import models.WikiContext

object MacroBr extends WikiFormattable {
  override def apply(body: String)(implicit wikiContext: WikiContext): String = "<br/>"
}
