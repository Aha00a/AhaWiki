package logics.wikis.macros

import models.WikiContext

import scala.util.matching.Regex

object MacroRuby extends TraitMacro {
  val regex: Regex = """^([^,]+),\s*([^,]+)$""".r

  override def apply(argument: String)(implicit wikiContext: WikiContext): String = {
    argument match {
      case regex(original, ruby) => s"<ruby><rb>$original</rb><rp>(</rp><rt>$ruby</rt><rp>)</rp></ruby>"
      case _ => MacroError(s"Argument Error - [[$name($argument)]]")
    }
  }

  override def calcLength(argument: String)(implicit wikiContext: WikiContext): Long = argument.length * 3 / 4
}
