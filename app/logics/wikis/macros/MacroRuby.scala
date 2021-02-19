package logics.wikis.macros

import models.ContextWikiPage

import scala.util.matching.Regex

object MacroRuby extends TraitMacro {
  val regex: Regex = """^([^,]+),\s*([^,]+)$""".r

  override def toHtmlString(argument: String)(implicit wikiContext: ContextWikiPage): String = {
    argument match {
      case regex(original, ruby) => s"<ruby><rb>$original</rb><rp>(</rp><rt>$ruby</rt><rp>)</rp></ruby>"
      case _ => MacroError.toHtmlString(s"Argument Error - [[$name($argument)]]")
    }
  }
}
