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
   * A page is being deleted and kept as `deletedPage`: its attachments are kept with it.
   *
   * Rows are marked Trashed and their objects stay in S3, so a restore only flips the rows back
   * ([[models.tables.Attachment.restoreTrashed]]). An object under the page's prefix that no row
   * holds cannot be restored -- nothing would name it -- so it is deleted now, as every object was
   * before pages were kept (2026-11-16).
   *
   * Rows are marked only once those deletes have succeeded. Marking first would leave a row
   * pointing at an object a failed delete had half dealt with.
   */
  def trashPageAttachments(siteSeq: Long, pageName: String, deletedPage: Long)
                          (implicit connection: Connection, applicationConf: ApplicationConf): Either[String, Int] = {
    val objectKeysFromDb = Attachment.selectObjectKeysByPage(siteSeq, pageName).map(_.trim).filter(_.nonEmpty).toSet
    val objectKeysFromS3 = listPageObjectKeys(siteSeq, pageName).map(_.trim).filter(_.nonEmpty).distinct
    val rowless = objectKeysFromS3.filterNot(objectKeysFromDb.contains)
    val objectKeysToDelete = rowless.filterNot(heldElsewhere(siteSeq, pageName, rowless).contains)

    deleteObjects(pageName, objectKeysToDelete).map { _ =>
      Attachment.markTrashed(siteSeq, pageName, deletedPage)
    }
  }

  /**
   * The keys of `objectKeys` that belong to something else and must be left alone: a live row of
   * another page, or a kept (deleted, restorable) page.
   *
   * listPageObjectKeys lists S3 by the sanitized page prefix, and sanitizing collapses distinct
   * names onto one prefix -- `A B` and `A_B` both become `A_B/` -- so the listing for one page
   * catches the other's objects. And a kept page's objects stay under its prefix, so a new page of
   * the same name lists them too.
   */
  def heldElsewhere(siteSeq: Long, pageName: String, objectKeys: Seq[String])(implicit connection: Connection): Set[String] =
    (Attachment.selectObjectKeysHeldByOtherPages(siteSeq, pageName, objectKeys) ++
      Attachment.selectTrashedObjectKeysAmong(siteSeq, objectKeys)).toSet

  /** Deletes a kept page's attachment objects for good, then marks their rows Deleted. */
  def purgeTrashedAttachments(deletedPage: Long, pageName: String)
                             (implicit connection: Connection, applicationConf: ApplicationConf): Either[String, Int] = {
    val objectKeys = Attachment.selectTrashedObjectKeys(deletedPage)
    deleteObjects(pageName, objectKeys).map(_ => objectKeys.map(Attachment.markDeleted).sum)
  }

  private def deleteObjects(pageName: String, objectKeys: Seq[String])(implicit applicationConf: ApplicationConf): Either[String, Unit] = {
    val failedObjectKeys =
      if (!S3Logic.isConfigured(applicationConf)) {
        Seq.empty
      } else {
        objectKeys.flatMap { objectKey =>
          Try(S3Logic.deleteObject(applicationConf, objectKey)) match {
            case Success(_) => None
            case Failure(error) =>
              logger.error(s"Attachment object delete failed. pageName=$pageName objectKey=$objectKey", error)
              Some(objectKey)
          }
        }
      }
    if (failedObjectKeys.nonEmpty) Left(s"Attachment delete failed. pageName=$pageName failedObjectKeys=${failedObjectKeys.mkString(",")}")
    else Right(())
  }
}
