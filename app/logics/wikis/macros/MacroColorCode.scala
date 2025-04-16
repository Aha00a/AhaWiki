package logics.wikis.macros

import models.ContextWikiPage

import scala.util.matching.Regex

object MacroColorCode extends TraitMacro {
  private val regexColorCodeSharpHex: Regex = """^(#[0-9A-Fa-f]{3}|#[0-9A-Fa-f]{4}|#[0-9A-Fa-f]{6}|[0-9A-Fa-f]{8})$""".r

  override def toHtmlString(argument: String)(implicit wikiContext: ContextWikiPage): String = {
    argument match {
      case "" => ""
      case regexColorCodeSharpHex(color) => <span class="MacroColorCode"><spen class="preview" style={s"""background-color: $color"""}></spen> {color}</span>.toString()
      case _ => MacroError.toHtmlString(s"Argument Error - [[$name($argument)]]")
    }
  }
}
