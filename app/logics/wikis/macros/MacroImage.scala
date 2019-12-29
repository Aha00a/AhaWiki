package logics.wikis.macros

import models.WikiContext

import scala.util.matching.Regex

object MacroImage extends TraitMacro {
  val regexWidth: Regex = """(.+),\s*(\d+(px|%)?)$""".r

  override def apply(argument: String)(implicit wikiContext: WikiContext): String = {
    //noinspection ScalaUnusedSymbol
    argument match {
      case regexWidth(url, width, null) => s"""<img src="$url" alt="$url" style="width: ${width}px"/>"""
      case regexWidth(url, width, unit) => s"""<img src="$url" alt="$url" style="width: $width"/>"""
      case _ => s"""<img src="$argument" alt="$argument"/>"""
    }
  }
}
