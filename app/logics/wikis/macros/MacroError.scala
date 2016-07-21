package logics.wikis.macros

import models.WikiContext

object MacroError extends TraitMacro {
  override def apply(argument: String)(implicit wikiContext: WikiContext): String = s"""<div class="error">$argument</div>"""
}
