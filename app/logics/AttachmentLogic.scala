package logics

import models.tables.Attachment
import play.api.Logging
import software.amazon.awssdk.services.s3.model.ListObjectsV2Request

import java.sql.Connection
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import scala.jdk.CollectionConverters._
import scala.util.Failure
import scala.util.Success
import scala.util.Try

/**
 * Where attachments live in S3, and the operations that walk that layout.
 *
 * The key layout is `Attachment/<siteSeq>/<pageName>/...`, with every segment sanitized the
 * same way. That layout is one fact: a reader that builds the prefix differently from the
 * writer finds nothing, and the mismatch does not show up until an attachment goes missing.
 * The writer's full key ([[objectKey]]) is built here for that reason, from the same page
 * prefix the listing reads; until 2026-10-04 it was a private method of `WikiAttachment`.
 *
 * The list and delete operations lived in both `Wiki` and `ApiV1` and had already drifted —
 * only the `ApiV1` copy checked whether S3 was configured. The guarded behaviour is the one
 * kept here, so an unconfigured S3 now yields "no attachments" instead of throwing on a
 * client with an empty region.
 */
object AttachmentLogic extends Logging {
  val Root: String = "Attachment"

  private val pathSegmentSanitizerRegex: String =
    "[^\\p{IsHangul}\\p{IsHan}\\p{IsHiragana}\\p{IsKatakana}a-zA-Z0-9._-]"

  private val listMaxKeys: Int = 200

  private val timestampFormatter: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH-mm-ss")

  def sanitizePathSegment(v: String): String = {
    val sanitized = v.replaceAll(pathSegmentSanitizerRegex, "_")
    if (sanitized.nonEmpty) sanitized else "_"
  }

  def sitePrefix(siteSeq: Long): String = s"$Root/$siteSeq/"

  def pagePrefix(siteSeq: Long, pageName: String): String =
    s"${sitePrefix(siteSeq)}${sanitizePathSegment(pageName)}/"

  /**
   * The key an upload is stored under: the page prefix, the file's name, then the name again with
   * the upload time before the extension. The time is what keeps a second upload of the same file
   * from replacing the first.
   */
  def objectKey(siteSeq: Long, pageName: String, originalFileName: String, extension: String, now: LocalDateTime = LocalDateTime.now()): String = {
    val sanitizedOriginalFileName = sanitizePathSegment(originalFileName)
    val sanitizedExtension = sanitizePathSegment(extension).toLowerCase
    val sanitizedOriginalFileNameWithoutExtension = {
      val stripped = sanitizedOriginalFileName.stripSuffix(s".$sanitizedExtension")
      if (stripped.nonEmpty) stripped else sanitizedOriginalFileName
    }
    s"${pagePrefix(siteSeq, pageName)}$sanitizedOriginalFileName/$sanitizedOriginalFileNameWithoutExtension.${now.format(timestampFormatter)}.$sanitizedExtension"
  }

  /** One page's S3 listing: the prefix it covers, the keys found, and whether S3 stopped short of the end. */
  case class PageListing(prefix: String, keys: Seq[String], truncated: Boolean)

  /** None when S3 is not configured. */
  def listPageObjects(siteSeq: Long, pageName: String)(implicit applicationConf: ApplicationConf): Option[PageListing] = {
    if (!S3Logic.isConfigured(applicationConf)) {
      None
    } else {
      val prefix = pagePrefix(siteSeq, pageName)
      val request = ListObjectsV2Request.builder()
        .bucket(S3Logic.bucket(applicationConf))
        .prefix(prefix)
        .maxKeys(listMaxKeys)
        .build()
      val response = S3Logic.client(applicationConf).listObjectsV2(request)
      val keys = response.contents().asScala.toSeq
        .map(_.key)
        .filter(key => key != null && key.nonEmpty && !key.endsWith("/"))
      // isTruncated is a java.lang.Boolean, and the SDK leaves it null when S3 sent no IsTruncated.
      Some(PageListing(prefix, keys, truncated = Option(response.isTruncated).exists(_.booleanValue)))
    }
  }

  def listPageObjectKeys(siteSeq: Long, pageName: String)(implicit applicationConf: ApplicationConf): Seq[String] =
    listPageObjects(siteSeq, pageName).map(_.keys).getOrElse(Seq.empty)

  /**
   * What the attachment list says about a row's object: `OK`, or `DB_ONLY` when there is reason to
   * think S3 does not have it.
   *
   * Signing a URL is local and says nothing about the object, so the page's listing decides — the
   * response reads it anyway, for `S3_ONLY`. A key is judged only by a listing that covers it: one
   * that S3 cut short may have stopped before it, and one for another prefix never looked. Either
   * way the row stays `OK`, as every row was before 2026-10-05.
   */
  def integrityStatus(objectKey: String, presigned: Boolean, listing: Option[PageListing]): String =
    if (!presigned) "DB_ONLY"
    else listing match {
      case Some(l) if !l.truncated && objectKey.startsWith(l.prefix) && !l.keys.contains(objectKey) => "DB_ONLY"
      case _ => "OK"
    }

  /**
   * Deletes every attachment object of a page, from S3 and then from the table.
   *
   * The table rows are marked deleted only once every object is gone. Marking them first
   * would leave an object with nothing pointing at it if a delete failed.
   */
  def deletePageAttachments(siteSeq: Long, pageName: String)
                           (implicit connection: Connection, applicationConf: ApplicationConf): Either[String, Unit] = {
    val objectKeysFromDb = Attachment.selectObjectKeysByPage(siteSeq, pageName)
    val objectKeysFromS3 = listPageObjectKeys(siteSeq, pageName)
    val objectKeys = (objectKeysFromDb ++ objectKeysFromS3).map(_.trim).filter(_.nonEmpty).distinct

    // listPageObjectKeys lists S3 by the sanitized page prefix, and sanitizing collapses distinct
    // names onto one prefix -- `A B` and `A_B` both become `A_B/` -- so the listing for one page
    // catches the other's objects. The DB rows are keyed by the exact page name and never collide,
    // so this only ever drops a key that came from the S3 listing and belongs to another page.
    val heldByOtherPages = Attachment.selectObjectKeysHeldByOtherPages(siteSeq, pageName, objectKeys).toSet
    val objectKeysToDelete = objectKeys.filterNot(heldByOtherPages.contains)

    val failedObjectKeys =
      if (!S3Logic.isConfigured(applicationConf)) {
        Seq.empty
      } else {
        objectKeysToDelete.flatMap { objectKey =>
          Try(S3Logic.deleteObject(applicationConf, objectKey)) match {
            case Success(_) => None
            case Failure(error) =>
              logger.error(s"deletePageAttachments failed. pageName=$pageName objectKey=$objectKey", error)
              Some(objectKey)
          }
        }
      }
    if (failedObjectKeys.nonEmpty) {
      Left(s"Attachment delete failed. pageName=$pageName failedObjectKeys=${failedObjectKeys.mkString(",")}")
    } else {
      objectKeysToDelete.foreach(Attachment.markDeleted)
      Right(())
    }
  }
}
