package logics.wikis.macros

import com.aha00a.commons.Implicits._
import models.ContextWikiPage

import scala.util.matching.Regex

object MacroAttachment extends TraitMacro {
  private val regexWidth: Regex = """(.+),\s*(\d+(px|%)?)$""".r
  private val imageExtensions: Set[String] = Set(
    "png",
    "jpg",
    "jpeg",
    "gif",
    "webp",
    "svg",
    "bmp",
    "avif",
    "tiff",
    "tif",
    "ico",
  )

  override def toHtmlString(argument: String)(implicit wikiContext: ContextWikiPage): String = {
    val (objectKey, widthOption) = argument match {
      case regexWidth(key, width, null) => (key.trim, Some(s"${width}px"))
      case regexWidth(key, width, unit) => (key.trim, Some(s"$width$unit"))
      case _ => (argument.trim, None)
    }

    if (objectKey.isEmpty) {
      return MacroError.toHtmlString("Attachment object key is empty.")
    }

    S3AttachmentUrlLogic.generatePresignedUrl(objectKey) match {
      case Left(errorMessage) =>
        MacroError.toHtmlString(s"Attachment($objectKey) - $errorMessage")
      case Right(url) =>
        val href = url.escapeHtmlAttribute()
        val fileName = objectKey.split("/").lastOption.getOrElse(objectKey)
        val extension = fileName.split('.').lastOption.map(_.toLowerCase).getOrElse("")
        val style = widthOption.map(width => s""" style="width: $width"""").getOrElse("")
        if (imageExtensions.contains(extension)) {
          s"""<img src="$href" alt="${fileName.escapeHtmlAttribute()}"$style/>"""
        } else {
          s"""<a href="$href" target="_blank" rel="noopener">${fileName.escapeHtml()}</a>"""
        }
    }
  }
}
