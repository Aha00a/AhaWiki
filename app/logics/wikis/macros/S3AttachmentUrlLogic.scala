package logics.wikis.macros

import com.amazonaws.HttpMethod
import com.amazonaws.auth.AWSStaticCredentialsProvider
import com.amazonaws.auth.BasicAWSCredentials
import com.amazonaws.services.s3.AmazonS3
import com.amazonaws.services.s3.AmazonS3ClientBuilder
import logics.ApplicationConf
import models.ContextWikiPage

import java.util.Date
import java.util.concurrent.ConcurrentHashMap
import scala.util.Try

object S3AttachmentUrlLogic {
  private case class S3ClientKey(region: String, accessKeyId: String, secretAccessKey: String)

  private val millisecondsOneDay: Long = 1000L * 60 * 60 * 24
  private val clients = new ConcurrentHashMap[S3ClientKey, AmazonS3]()

  private def s3Client(applicationConf: ApplicationConf): AmazonS3 = {
    val key = S3ClientKey(
      applicationConf.AhaWiki.aws.AWS_REGION(),
      applicationConf.AhaWiki.aws.AWS_ACCESS_KEY_ID(),
      applicationConf.AhaWiki.aws.AWS_SECRET_ACCESS_KEY(),
    )
    clients.computeIfAbsent(key, _ => {
      val credentials = new BasicAWSCredentials(key.accessKeyId, key.secretAccessKey)
      AmazonS3ClientBuilder.standard
        .withCredentials(new AWSStaticCredentialsProvider(credentials))
        .withRegion(key.region)
        .build()
    })
  }

  def generatePresignedUrl(applicationConf: ApplicationConf, objectKey: String): Either[String, String] = {
    Try {
      val bucket = applicationConf.AhaWiki.aws.s3.bucket()
      val expiration = new Date(System.currentTimeMillis() + millisecondsOneDay)
      s3Client(applicationConf).generatePresignedUrl(bucket, objectKey, expiration, HttpMethod.GET).toString
    }.toEither.left.map(_.getMessage)
  }

  def generatePresignedUrl(objectKey: String)(implicit wikiContext: ContextWikiPage): Either[String, String] = {
    generatePresignedUrl(wikiContext.applicationConf, objectKey)
  }
}
