package logics.wikis.macros

import models.WikiContext

object MacroCopyable extends TraitMacro {

  override def toHtmlString(argument: String)(implicit wikiContext: WikiContext): String = {
    <input value={argument} class="MacroCopyable" readonly="readonly"/>.toString()
  }
}
