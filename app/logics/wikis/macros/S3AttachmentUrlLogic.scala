package logics.wikis.macros

import logics.ApplicationConf
import logics.S3Logic
import models.ContextWikiPage
import software.amazon.awssdk.services.s3.model.GetObjectRequest
import software.amazon.awssdk.services.s3.presigner.model.GetObjectPresignRequest

import java.time.Duration
import scala.util.Try

object S3AttachmentUrlLogic {
  private val validFor: Duration = Duration.ofDays(1)

  /** A URL that reads the object for a day. Signing is local: it does not ask S3 whether the
    * object exists. Left with the reason when it cannot be signed, as when S3 is not configured. */
  def generatePresignedUrl(applicationConf: ApplicationConf, objectKey: String): Either[String, String] = {
    Try {
      val getObjectRequest = GetObjectRequest.builder().bucket(S3Logic.bucket(applicationConf)).key(objectKey).build()
      val presignRequest = GetObjectPresignRequest.builder().signatureDuration(validFor).getObjectRequest(getObjectRequest).build()
      S3Logic.presigner(applicationConf).presignGetObject(presignRequest).url().toString
    }.toEither.left.map(_.getMessage)
  }

  def generatePresignedUrl(objectKey: String)(implicit wikiContext: ContextWikiPage): Either[String, String] = {
    generatePresignedUrl(wikiContext.applicationConf, objectKey)
  }
}
