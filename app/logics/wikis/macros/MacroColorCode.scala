package logics.wikis.macros

import models.ContextWikiPage

import scala.util.matching.Regex

object MacroColorCode extends TraitMacro {
  private val regexColorCodeSharpHex: Regex = """^(#[0-9A-Fa-f]{3}|#[0-9A-Fa-f]{4}|#[0-9A-Fa-f]{6}|[0-9A-Fa-f]{8})$""".r

  override def toHtmlString(argument: String)(implicit wikiContext: ContextWikiPage): String = {
    argument match {
      case "" => ""
      case regexColorCodeSharpHex(color) =>
        val swatch = s"""<span class="MacroColorCodeSwatch"><span style="background: $color"></span></span>"""
        s"""<span class="MacroColorCode">$swatch${MacroCopyable.doToHtmlString(color)}</span>"""
      case _ => MacroError.toHtmlString(s"Argument Error - [[$name($argument)]]")
    }
  }
}
