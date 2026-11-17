package logics.wikis.macros

import models.ContextWikiPage

import scala.util.matching.Regex

object MacroColorCode extends TraitMacro {
  // The eight-digit form is taken with or without `#`. Until 2026-09-12 it was taken only
  // without, and a CSS colour needs the `#`, so its swatch came out empty; the `#` is added.
  private val regexColorCodeSharpHex: Regex = """^(#[0-9A-Fa-f]{3}|#[0-9A-Fa-f]{4}|#[0-9A-Fa-f]{6}|#?[0-9A-Fa-f]{8})$""".r

  // CSS's colour functions (2026-11-18): rgb() and hsl() with their -a forms, oklch() and the
  // other spaces the browser draws, hwb(), lab(), lch(), oklab(). Inside the parentheses only
  // numbers -- with a sign, a fraction, an exponent, and %, deg, rad, grad or turn -- `none`, and
  // the separators: space, comma, slash. The value goes into a style attribute as written, so
  // nothing else may reach it: no quote, semicolon, parenthesis or var(). Whether the numbers make
  // a colour is the browser's to say; one that does not leaves the swatch showing its checkerboard,
  // as an eight-digit hex without `#` used to. The macro's own regex reads up to `)]]`, which is
  // why the function's closing parenthesis gets through.
  private val number = """[-+]?(?:\d+(?:\.\d*)?|\.\d+)(?:[eE][-+]?\d+)?(?:%|deg|rad|grad|turn)?"""
  private val regexColorFunction: Regex =
    s"""(?i)^(rgba?|hsla?|hwb|lab|lch|oklab|oklch)\\(\\s*((?:$number|none)(?:\\s*[,/]?\\s*(?:$number|none))*)\\s*\\)$$""".r

  override def toHtmlString(argument: String)(implicit wikiContext: ContextWikiPage): String = {
    argument.trim match {
      case "" => ""
      case regexColorCodeSharpHex(written) =>
        render(if (written.startsWith("#")) written else s"#$written")
      case regexColorFunction(function, values) =>
        render(s"${function.toLowerCase}(${values.replaceAll("""\s+""", " ")})")
      case _ => argumentError(argument)
    }
  }

  private def render(color: String): String = {
    val swatch = s"""<span class="MacroColorCodeSwatch"><span style="background: $color"></span></span>"""
    s"""<span class="MacroColorCode">$swatch${MacroCopyable.doToHtmlString(color)}</span>"""
  }
}
