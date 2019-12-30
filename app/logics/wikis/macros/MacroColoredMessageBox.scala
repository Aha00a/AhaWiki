package logics.wikis.macros

import models.WikiContext

trait MacroColoredMessageBox extends TraitMacro {
  val cls: String
  override def apply(argument: String)(implicit wikiContext: WikiContext): String = {
    s"""<div class="${cls}">${argument}</div>"""
  }
}
