package logics.wikis.macros

import com.amazonaws.HttpMethod
import logics.ApplicationConf
import logics.S3Logic
import models.ContextWikiPage

import java.util.Date
import scala.util.Try

object S3AttachmentUrlLogic {
  private val millisecondsOneDay: Long = 1000L * 60 * 60 * 24

  def generatePresignedUrl(applicationConf: ApplicationConf, objectKey: String): Either[String, String] = {
    Try {
      val bucket = S3Logic.bucket(applicationConf)
      val expiration = new Date(System.currentTimeMillis() + millisecondsOneDay)
      S3Logic.client(applicationConf).generatePresignedUrl(bucket, objectKey, expiration, HttpMethod.GET).toString
    }.toEither.left.map(_.getMessage)
  }

  def generatePresignedUrl(objectKey: String)(implicit wikiContext: ContextWikiPage): Either[String, String] = {
    generatePresignedUrl(wikiContext.applicationConf, objectKey)
  }
}
