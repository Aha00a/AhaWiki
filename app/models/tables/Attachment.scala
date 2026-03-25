package models.tables

import anorm._

import java.sql.Connection

object Attachment {
  def insertInitiated(
    site: Long,
    pageName: String,
    user: Option[Int],
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
}
