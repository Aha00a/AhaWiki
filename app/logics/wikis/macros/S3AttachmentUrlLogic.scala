package logics.wikis.macros

import com.amazonaws.HttpMethod
import com.amazonaws.auth.AWSStaticCredentialsProvider
import com.amazonaws.auth.BasicAWSCredentials
import com.amazonaws.services.s3.AmazonS3
import com.amazonaws.services.s3.AmazonS3ClientBuilder
import logics.ApplicationConf
import models.ContextWikiPage

import java.util.Date
import scala.util.Try

object S3AttachmentUrlLogic {
  private val millisecondsOneDay: Long = 1000L * 60 * 60 * 24

  def generatePresignedUrl(applicationConf: ApplicationConf, objectKey: String): Either[String, String] = {
    Try {
      val credentials = new BasicAWSCredentials(
        applicationConf.AhaWiki.aws.AWS_ACCESS_KEY_ID(),
        applicationConf.AhaWiki.aws.AWS_SECRET_ACCESS_KEY(),
      )
      val amazonS3: AmazonS3 = AmazonS3ClientBuilder.standard
        .withCredentials(new AWSStaticCredentialsProvider(credentials))
        .withRegion(applicationConf.AhaWiki.aws.AWS_REGION())
        .build()

      val bucket = applicationConf.AhaWiki.aws.s3.bucket()
      val expiration = new Date(System.currentTimeMillis() + millisecondsOneDay)
      amazonS3.generatePresignedUrl(bucket, objectKey, expiration, HttpMethod.GET).toString
    }.toEither.left.map(_.getMessage)
  }

  def generatePresignedUrl(objectKey: String)(implicit wikiContext: ContextWikiPage): Either[String, String] = {
    generatePresignedUrl(wikiContext.applicationConf, objectKey)
  }
}
