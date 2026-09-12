package logics.wikis.macros

import com.aha00a.commons.Implicits._
import logics.AttachmentLogic
import models.ContextWikiPage

import scala.util.matching.Regex

object MacroAttachment extends TraitMacro {
  private val fallbackImagePath: String = "/assets/img/attachmentFallback.svg"
  private val regexWidth: Regex = """(.+),\s*(\d+(px|%)?)$""".r
  private val attachmentRoot: String = AttachmentLogic.Root
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

  private def normalizeObjectKey(rawObjectKey: String, siteSeq: Long, pageName: String): String = {
    val trimmed = rawObjectKey.trim.stripPrefix("/")
    if (trimmed.startsWith(s"$attachmentRoot/")) {
      trimmed
    } else {
      val segments = trimmed.split("/").toSeq.filter(_.nonEmpty)
      segments match {
        case Seq("clipboard", _*) =>
          val sanitizedPageName = AttachmentLogic.sanitizePathSegment(pageName)
          s"$attachmentRoot/$siteSeq/$sanitizedPageName/$trimmed"
        case Seq(pn, "clipboard", rest @ _*) =>
          val normalizedPagePath = (Seq(AttachmentLogic.sanitizePathSegment(pn), "clipboard") ++ rest).mkString("/")
          s"$attachmentRoot/$siteSeq/$normalizedPagePath"
        case Seq(fileName, rest @ _*) =>
          val sanitizedPageName = AttachmentLogic.sanitizePathSegment(pageName)
          val normalizedPagePath = (Seq(sanitizedPageName, AttachmentLogic.sanitizePathSegment(fileName)) ++ rest.map(AttachmentLogic.sanitizePathSegment)).mkString("/")
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

  /**
   * The object key, and the CSS width if one was given; a bare number is pixels.
   *
   * The width group already holds its unit. Until 2026-09-12 the unit was appended again, so
   * `200px` became `width: 200pxpx` and `50%` became `50%%`, which the browser drops -- only a
   * bare number ever set a width. MacroImage had the same line and was fixed on 2026-05-20.
   */
  def parseArgument(argument: String): (String, Option[String]) = argument match {
    case regexWidth(key, width, null) => (key.trim, Some(s"${width}px"))
    case regexWidth(key, width, _) => (key.trim, Some(width))
    case _ => (argument.trim, None)
  }

  override def toHtmlString(argument: String)(implicit wikiContext: ContextWikiPage): String = {
    val (rawObjectKey, widthOption) = parseArgument(argument)
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
