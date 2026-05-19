package logics.wikis.macros

import com.aha00a.commons.Implicits._
import models.ContextWikiPage

import scala.util.matching.Regex

object MacroImage extends TraitMacro {
  private val fallbackImagePath: String = "/assets/img/attachmentFallback.svg"
  private val regexWidth: Regex = """(.+),\s*(\d+(px|%)?)$""".r

  private def imageTag(src: String, alt: String, widthOption: Option[String]): String = {
    val href = src.escapeHtmlAttribute()
    val escapedAlt = alt.escapeHtmlAttribute()
    val fallbackImageSrc = fallbackImagePath.escapeHtmlAttribute()
    val style = widthOption.map(width => s""" style="width: $width"""").getOrElse("")
    s"""<img src="$href" alt="$escapedAlt" onerror="this.onerror=null;this.src='$fallbackImageSrc';"$style/>"""
  }

  override def toHtmlString(argument: String)(implicit wikiContext: ContextWikiPage): String = {
    val (url, widthOption) = argument match {
      case regexWidth(rawUrl, width, null) => (rawUrl.trim, Some(s"${width}px"))
      case regexWidth(rawUrl, width, unit) => (rawUrl.trim, Some(s"$width$unit"))
      case _ => (argument.trim, None)
    }
    val src = if (url.isEmpty) fallbackImagePath else url
    imageTag(src, src, widthOption)
  }
}
