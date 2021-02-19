package logics.wikis.macros

import models.ContextWikiPage

trait MacroColoredMessageBox extends TraitMacro {
  val cls: String
  override def toHtmlString(argument: String)(implicit wikiContext: ContextWikiPage): String = {
    s"""<div class="$cls">$argument</div>"""
  }
}
