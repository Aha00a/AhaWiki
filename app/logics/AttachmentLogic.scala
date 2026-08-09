package logics

import com.amazonaws.services.s3.model.ListObjectsV2Request
import models.tables.Attachment
import play.api.Logging

import java.sql.Connection
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
 *
 * The list and delete operations lived in both `Wiki` and `ApiV1` and had already drifted —
 * only the `ApiV1` copy checked whether S3 was configured. The guarded behaviour is the one
 * kept here, so an unconfigured S3 now yields "no attachments" instead of throwing while a
 * client is built with an empty region.
 */
object AttachmentLogic extends Logging {
  val Root: String = "Attachment"

  private val pathSegmentSanitizerRegex: String =
    "[^\\p{IsHangul}\\p{IsHan}\\p{IsHiragana}\\p{IsKatakana}a-zA-Z0-9._-]"

  private val listMaxKeys: Int = 200

  def sanitizePathSegment(v: String): String = {
    val sanitized = v.replaceAll(pathSegmentSanitizerRegex, "_")
    if (sanitized.nonEmpty) sanitized else "_"
  }

  def sitePrefix(siteSeq: Long): String = s"$Root/$siteSeq/"

  def pagePrefix(siteSeq: Long, pageName: String): String =
    s"${sitePrefix(siteSeq)}${sanitizePathSegment(pageName)}/"

  def listPageObjectKeys(siteSeq: Long, pageName: String)(implicit applicationConf: ApplicationConf): Seq[String] = {
    if (!S3Logic.isConfigured(applicationConf)) {
      Seq.empty
    } else {
      val request = new ListObjectsV2Request()
        .withBucketName(S3Logic.bucket(applicationConf))
        .withPrefix(pagePrefix(siteSeq, pageName))
        .withMaxKeys(listMaxKeys)
      S3Logic.client(applicationConf).listObjectsV2(request).getObjectSummaries.asScala.toSeq
        .map(_.getKey)
        .filter(key => key != null && key.nonEmpty && !key.endsWith("/"))
    }
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

    val failedObjectKeys =
      if (!S3Logic.isConfigured(applicationConf)) {
        Seq.empty
      } else {
        val bucket = S3Logic.bucket(applicationConf)
        val amazonS3 = S3Logic.client(applicationConf)
        objectKeys.flatMap { objectKey =>
          Try(amazonS3.deleteObject(bucket, objectKey)) match {
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
      objectKeys.foreach(Attachment.markDeleted)
      Right(())
    }
  }
}
