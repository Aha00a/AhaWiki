package logics.wikis.macros

import com.aha00a.commons.Implicits._
import models.ContextWikiPage

import scala.util.matching.Regex

object MacroAttachment extends TraitMacro {
  private val fallbackImagePath: String = "/assets/img/attachmentFallback.svg"
  private val regexWidth: Regex = """(.+),\s*(\d+(px|%)?)$""".r
  private val attachmentRoot: String = "Attachment"
  private val attachmentPathSanitizerRegex: String = "[^\\p{IsHangul}\\p{IsHan}\\p{IsHiragana}\\p{IsKatakana}a-zA-Z0-9._-]"
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

  private def sanitizeAttachmentPathSegment(v: String): String = {
    val sanitized = v.replaceAll(attachmentPathSanitizerRegex, "_")
    if (sanitized.nonEmpty) sanitized else "_"
  }

  private def normalizeObjectKey(rawObjectKey: String, siteSeq: Long, pageName: String): String = {
    val trimmed = rawObjectKey.trim.stripPrefix("/")
    if (trimmed.startsWith(s"$attachmentRoot/")) {
      trimmed
    } else {
      val segments = trimmed.split("/").toSeq.filter(_.nonEmpty)
      segments match {
        case Seq("clipboard", _*) =>
          val sanitizedPageName = sanitizeAttachmentPathSegment(pageName)
          s"$attachmentRoot/$siteSeq/$sanitizedPageName/$trimmed"
        case Seq(pn, "clipboard", rest @ _*) =>
          val normalizedPagePath = (Seq(sanitizeAttachmentPathSegment(pn), "clipboard") ++ rest).mkString("/")
          s"$attachmentRoot/$siteSeq/$normalizedPagePath"
        case Seq(fileName, rest @ _*) =>
          val sanitizedPageName = sanitizeAttachmentPathSegment(pageName)
          val normalizedPagePath = (Seq(sanitizedPageName, sanitizeAttachmentPathSegment(fileName)) ++ rest.map(sanitizeAttachmentPathSegment)).mkString("/")
          s"$attachmentRoot/$siteSeq/$normalizedPagePath"
        case _ =>
          trimmed
      }
    }
  }

  private def normalizeObjectKey(rawObjectKey: String)(implicit wikiContext: ContextWikiPage): String =
    normalizeObjectKey(rawObjectKey, wikiContext.site.seq, wikiContext.name)

  def toAttachmentUri(rawObjectKey: String, siteSeq: Long, pageName: String): String = {
    val objectKey = normalizeObjectKey(rawObjectKey, siteSeq, pageName)
    s"attachment:${objectKey.stripPrefix(s"$attachmentRoot/$siteSeq/")}"
  }

  override def toHtmlString(argument: String)(implicit wikiContext: ContextWikiPage): String = {
    val (rawObjectKey, widthOption) = argument match {
      case regexWidth(key, width, null) => (key.trim, Some(s"${width}px"))
      case regexWidth(key, width, unit) => (key.trim, Some(s"$width$unit"))
      case _ => (argument.trim, None)
    }
    val objectKey = normalizeObjectKey(rawObjectKey)

    if (rawObjectKey.isEmpty) {
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
          val fallbackImageSrc = fallbackImagePath.escapeHtmlAttribute()
          s"""<img src="$href" alt="${fileName.escapeHtmlAttribute()}" onerror="this.onerror=null;this.src='$fallbackImageSrc';"$style/>"""
        } else {
          s"""<a href="$href" target="_blank" rel="noopener">${fileName.escapeHtml()}</a>"""
        }
    }
  }
}
