package logics.wikis.macros

import models.ContextWikiPage

object MacroCheckBox extends TraitMacro {
  override def toHtmlString(argument: String)(implicit wikiContext: ContextWikiPage): String = {
    val checked = if (argument != null && argument.nonEmpty) " checked" else ""
    s"""<input type="checkbox" disabled$checked>"""
  }
}
