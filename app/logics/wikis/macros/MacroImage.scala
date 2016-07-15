package logics.wikis.macros

import models.WikiContext

object MacroImage extends TraitMacro {
  val regexWidth = """(.+),\s*(\d+(px|%)?)$""".r

  override def apply(argument: String)(implicit wikiContext: WikiContext): String = {
    argument match {
      case regexWidth(url, width, null) => s"""<img src="$url" alt="$url" style="width: ${width}px"/>"""
      case regexWidth(url, width, unit) => s"""<img src="$url" alt="$url" style="width: $width"/>"""
      case _ => s"""<img src="$argument" alt="$argument"/>"""
    }
  }

  override def calcLength(argument: String)(implicit wikiContext: WikiContext): Long = argument.length * 3 / 4
}
