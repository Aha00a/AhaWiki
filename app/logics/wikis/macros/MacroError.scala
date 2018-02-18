package logics.wikis.macros

import models.WikiContext

object MacroError extends TraitMacro {
  override def apply(argument: String)(implicit wikiContext: WikiContext): String = s"""<pre class="error">$argument</pre>"""
}
