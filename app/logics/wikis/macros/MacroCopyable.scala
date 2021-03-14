package logics.wikis.macros

import models.ContextWikiPage

object MacroCopyable extends TraitMacro {
  override def toHtmlString(argument: String)(implicit wikiContext: ContextWikiPage): String = {
    <input value={argument} class="MacroCopyable" readonly="readonly"/>.toString()
  }
}
