package logics.wikis.macros

import models.ContextWikiPage

import scala.util.matching.Regex

object MacroColorCode extends TraitMacro {
  // The eight-digit form is taken with or without `#`. Until 2026-09-12 it was taken only
  // without, and a CSS colour needs the `#`, so its swatch came out empty; the `#` is added.
  private val regexColorCodeSharpHex: Regex = """^(#[0-9A-Fa-f]{3}|#[0-9A-Fa-f]{4}|#[0-9A-Fa-f]{6}|#?[0-9A-Fa-f]{8})$""".r

  override def toHtmlString(argument: String)(implicit wikiContext: ContextWikiPage): String = {
    argument match {
      case "" => ""
      case regexColorCodeSharpHex(written) =>
        val color = if (written.startsWith("#")) written else s"#$written"
        val swatch = s"""<span class="MacroColorCodeSwatch"><span style="background: $color"></span></span>"""
        s"""<span class="MacroColorCode">$swatch${MacroCopyable.doToHtmlString(color)}</span>"""
      case _ => argumentError(argument)
    }
  }
}
