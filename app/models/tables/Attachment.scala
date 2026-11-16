package models.tables

import anorm._

import java.sql.Connection
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import anorm.SqlParser.{flatten, long, str}

case class AttachmentRow(
  seq: Long,
  dateInserted: LocalDateTime,
  pageName: String,
  originalFilename: String,
  objectKey: String,
  contentType: String,
  fileSize: Long,
  status: String,
)

object Attachment {
  private val dateTimeParserCandidates: Seq[DateTimeFormatter] = Seq(
    DateTimeFormatter.ISO_LOCAL_DATE_TIME,
    DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"),
  )

  private def parseLocalDateTime(raw: String): LocalDateTime = {
    val trimmed = Option(raw).map(_.trim).getOrElse("")
    dateTimeParserCandidates.view.flatMap { formatter =>
      scala.util.Try(LocalDateTime.parse(trimmed, formatter)).toOption
    }.headOption.getOrElse(throw new IllegalArgumentException(s"Invalid dateInserted: $trimmed"))
  }

  private val attachmentRowParser: RowParser[AttachmentRow] = {
    (long("seq") ~ str("dateInserted") ~ str("pageName") ~ str("originalFilename") ~ str("objectKey") ~ str("contentType") ~ long("fileSize") ~ str("status")).map {
      case seq ~ dateInsertedRaw ~ pageName ~ originalFilename ~ objectKey ~ contentType ~ fileSize ~ status =>
        AttachmentRow(
          seq = seq,
          dateInserted = parseLocalDateTime(dateInsertedRaw),
          pageName = pageName,
          originalFilename = originalFilename,
          objectKey = objectKey,
          contentType = contentType,
          fileSize = fileSize,
          status = status,
        )
    }
  }

  def insertInitiated(
    site: Long,
    pageName: String,
    user: Option[Long],
    uploaderEmail: Option[String],
    originalFilename: String,
    storedFilename: String,
    bucket: String,
    objectKey: String,
    contentType: String,
    fileSize: Long,
  )(implicit connection: Connection): Option[Long] = {
    SQL"""
      INSERT INTO Attachment
        (site, pageName, user, uploaderEmail, originalFilename, storedFilename, bucket, objectKey, contentType, fileSize, status)
      VALUES
        ($site, $pageName, $user, $uploaderEmail, $originalFilename, $storedFilename, $bucket, $objectKey, $contentType, $fileSize, 'Initiated')
    """.executeInsert()
  }

  def markUploaded(
    objectKey: String,
    etag: Option[String],
  )(implicit connection: Connection): Int = {
    SQL"""
      UPDATE Attachment
      SET
        status = 'Uploaded',
        etag = $etag,
        dateUploaded = NOW(),
        dateUpdated = NOW()
      WHERE objectKey = $objectKey
    """.executeUpdate()
  }

  def markFailed(objectKey: String)(implicit connection: Connection): Int = {
    SQL"""
      UPDATE Attachment
      SET
        status = 'Failed',
        dateUpdated = NOW()
      WHERE objectKey = $objectKey
    """.executeUpdate()
  }

  def selectUploadedByPage(siteSeq: Long, pageName: String)(implicit connection: Connection): Seq[AttachmentRow] = {
    SQL"""
      SELECT
        seq,
        DATE_FORMAT(dateInserted, '%Y-%m-%d %H:%i:%s') AS dateInserted,
        pageName,
        originalFilename,
        objectKey,
        contentType,
        fileSize,
        status
      FROM Attachment
      WHERE site = $siteSeq
        AND pageName = $pageName
        AND status IN ('Uploaded', 'Verified')
        AND dateDeleted IS NULL
      ORDER BY dateInserted DESC
    """.as(attachmentRowParser.*)
  }

  def selectByObjectKey(siteSeq: Long, pageName: String, objectKey: String)(implicit connection: Connection): Option[AttachmentRow] = {
    SQL"""
      SELECT
        seq,
        DATE_FORMAT(dateInserted, '%Y-%m-%d %H:%i:%s') AS dateInserted,
        pageName,
        originalFilename,
        objectKey,
        contentType,
        fileSize,
        status
      FROM Attachment
      WHERE site = $siteSeq
        AND pageName = $pageName
        AND objectKey = $objectKey
        AND dateDeleted IS NULL
      LIMIT 1
    """.as(attachmentRowParser.singleOpt)
  }

  def markDeleted(objectKey: String)(implicit connection: Connection): Int = {
    SQL"""
      UPDATE Attachment
      SET
        status = 'Deleted',
        dateDeleted = NOW(),
        dateUpdated = NOW()
      WHERE objectKey = $objectKey
    """.executeUpdate()
  }

  /**
   * The page's live rows become Trashed and point at the kept page. The objects stay in S3, so a
   * restore only has to undo this. dateDeleted is set as well, so every reader of live rows
   * (dateDeleted IS NULL) leaves them out without knowing about Trashed.
   */
  def markTrashed(siteSeq: Long, pageName: String, deletedPage: Long)(implicit connection: Connection): Int = {
    SQL"""
      UPDATE Attachment
      SET
        status = 'Trashed',
        deletedPage = $deletedPage,
        dateDeleted = NOW(),
        dateUpdated = NOW()
      WHERE site = $siteSeq
        AND pageName = $pageName
        AND dateDeleted IS NULL
    """.executeUpdate()
  }

  /** Undoes [[markTrashed]]. A restored row reads Uploaded whatever it read before. */
  def restoreTrashed(deletedPage: Long)(implicit connection: Connection): Int = {
    SQL"""
      UPDATE Attachment
      SET
        status = 'Uploaded',
        deletedPage = NULL,
        dateDeleted = NULL,
        dateUpdated = NOW()
      WHERE deletedPage = $deletedPage
        AND status = 'Trashed'
    """.executeUpdate()
  }

  def selectTrashedObjectKeys(deletedPage: Long)(implicit connection: Connection): Seq[String] = {
    SQL"""
      SELECT objectKey
      FROM Attachment
      WHERE deletedPage = $deletedPage
        AND status = 'Trashed'
    """.as(str("objectKey").*)
  }

  /**
   * Of the given keys, the ones a kept (deleted, restorable) page still holds on this site.
   *
   * A kept page's objects stay under the page's S3 prefix, so a new page of the same name lists
   * them as its own. Its attachment list must not show them as S3_ONLY, and its delete must not
   * remove them.
   */
  def selectTrashedObjectKeysAmong(siteSeq: Long, objectKeys: Seq[String])(implicit connection: Connection): Seq[String] = {
    if (objectKeys.isEmpty) {
      Seq.empty
    } else {
      SQL"""
        SELECT objectKey
        FROM Attachment
        WHERE site = $siteSeq
          AND status = 'Trashed'
          AND objectKey IN ($objectKeys)
      """.as(str("objectKey").*)
    }
  }

  def selectObjectKeysByPage(siteSeq: Long, pageName: String)(implicit connection: Connection): Seq[String] = {
    SQL"""
      SELECT objectKey
      FROM Attachment
      WHERE site = $siteSeq
        AND pageName = $pageName
        AND dateDeleted IS NULL
    """.as(str("objectKey").*)
  }

  /**
   * Of the given keys, the ones a live row of another page on this site still holds.
   *
   * A page delete lists S3 by the sanitized page prefix, and sanitizing collapses distinct names
   * onto one prefix, so `A B`'s listing catches `A_B`'s objects. This is how the caller keeps from
   * deleting them: a key here belongs to another page and must be left alone.
   */
  def selectObjectKeysHeldByOtherPages(siteSeq: Long, pageName: String, objectKeys: Seq[String])(implicit connection: Connection): Seq[String] = {
    if (objectKeys.isEmpty) {
      Seq.empty
    } else {
      SQL"""
        SELECT objectKey
        FROM Attachment
        WHERE site = $siteSeq
          AND pageName <> $pageName
          AND dateDeleted IS NULL
          AND objectKey IN ($objectKeys)
      """.as(str("objectKey").*)
    }
  }
}
