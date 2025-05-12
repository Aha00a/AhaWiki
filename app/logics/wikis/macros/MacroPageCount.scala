package logics.wikis.macros

import models.ContextWikiPage

object MacroPageCount extends TraitMacro {
  override def toHtmlString(argument:String)(implicit wikiContext: ContextWikiPage): String = wikiContext.seqPageByPermission.size.toString
}
