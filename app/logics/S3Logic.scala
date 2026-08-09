package logics

import com.amazonaws.auth.AWSStaticCredentialsProvider
import com.amazonaws.auth.BasicAWSCredentials
import com.amazonaws.services.s3.AmazonS3
import com.amazonaws.services.s3.AmazonS3ClientBuilder

import java.util.concurrent.ConcurrentHashMap

/**
 * Access to S3: whether it is configured, and the client to use.
 *
 * Clients are cached by credentials. `AmazonS3` holds a connection pool and is built to be
 * shared, so building one per request — which three controllers each used to do — spends a
 * pool per request and never releases it. Nothing here mutates or closes a client, so one
 * instance per credential set is safe to hand out.
 */
object S3Logic {
  private case class ClientKey(region: String, accessKeyId: String, secretAccessKey: String)

  private val clients = new ConcurrentHashMap[ClientKey, AmazonS3]()

  def isConfigured(applicationConf: ApplicationConf): Boolean =
    Seq(
      applicationConf.AhaWiki.aws.AWS_REGION(),
      applicationConf.AhaWiki.aws.AWS_ACCESS_KEY_ID(),
      applicationConf.AhaWiki.aws.AWS_SECRET_ACCESS_KEY(),
      applicationConf.AhaWiki.aws.s3.bucket(),
    ).forall(_.trim.nonEmpty)

  def bucket(applicationConf: ApplicationConf): String = applicationConf.AhaWiki.aws.s3.bucket()

  def client(applicationConf: ApplicationConf): AmazonS3 = {
    val key = ClientKey(
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
}
