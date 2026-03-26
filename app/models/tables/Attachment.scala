package models.tables

import anorm._

import java.sql.Connection
import java.time.LocalDateTime

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
  private val attachmentRowParser: RowParser[AttachmentRow] = Macro.namedParser[AttachmentRow]

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
        dateInserted,
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
        dateInserted,
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
}
