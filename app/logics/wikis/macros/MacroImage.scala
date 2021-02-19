package logics.wikis.macros

import models.ContextWikiPage

import scala.util.matching.Regex

object MacroImage extends TraitMacro {
  val regexWidth: Regex = """(.+),\s*(\d+(px|%)?)$""".r

  override def toHtmlString(argument: String)(implicit wikiContext: ContextWikiPage): String = {
    //noinspection ScalaUnusedSymbol
    argument match {
      case regexWidth(url, width, null) => s"""<img src="$url" alt="$url" style="width: ${width}px"/>"""
      case regexWidth(url, width, unit) => s"""<img src="$url" alt="$url" style="width: $width"/>"""
      case _ => s"""<img src="$argument" alt="$argument"/>"""
    }
  }
}
