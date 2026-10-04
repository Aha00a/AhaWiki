package logics

import software.amazon.awssdk.auth.credentials.AwsBasicCredentials
import software.amazon.awssdk.auth.credentials.StaticCredentialsProvider
import software.amazon.awssdk.core.sync.RequestBody
import software.amazon.awssdk.http.apache5.Apache5HttpClient
import software.amazon.awssdk.regions.Region
import software.amazon.awssdk.services.s3.S3Client
import software.amazon.awssdk.services.s3.model.Delete
import software.amazon.awssdk.services.s3.model.DeleteObjectRequest
import software.amazon.awssdk.services.s3.model.DeleteObjectsRequest
import software.amazon.awssdk.services.s3.model.ObjectIdentifier
import software.amazon.awssdk.services.s3.model.PutObjectRequest
import software.amazon.awssdk.services.s3.presigner.S3Presigner

import java.net.URI
import java.nio.file.Path
import java.util.concurrent.ConcurrentHashMap
import scala.jdk.CollectionConverters._

/**
 * Access to S3: whether it is configured, the client to use, and the uploads and deletes made
 * through it.
 *
 * Clients are cached by credentials. `S3Client` holds a connection pool and is built to be
 * shared, so building one per request — which three controllers each used to do — spends a
 * pool per request and never releases it. Nothing here mutates or closes a client, so one
 * instance per credential set is safe to hand out. The presigner is cached the same way.
 *
 * Neither is built while S3 is not configured: [[client]] and [[presigner]] throw instead, with
 * [[NotConfigured]] as the message. AWS SDK 1.x built a client from an empty region without
 * complaint, and the failure came later and said less — signing a URL with it failed with
 * "Endpoint does not contain a valid host name: null".
 *
 * The HTTP client is chosen here rather than left to the SDK, which picks whichever
 * implementation it finds on the classpath. Wiki page Dev Attachment has why it is Apache 5.
 */
object S3Logic {
  val NotConfigured: String = "S3 is not configured."

  private[logics] case class ClientKey(region: String, accessKeyId: String, secretAccessKey: String)

  private val clients = new ConcurrentHashMap[ClientKey, S3Client]()
  private val presigners = new ConcurrentHashMap[ClientKey, S3Presigner]()

  def isConfigured(applicationConf: ApplicationConf): Boolean =
    Seq(
      applicationConf.AhaWiki.aws.AWS_REGION(),
      applicationConf.AhaWiki.aws.AWS_ACCESS_KEY_ID(),
      applicationConf.AhaWiki.aws.AWS_SECRET_ACCESS_KEY(),
      applicationConf.AhaWiki.aws.s3.bucket(),
    ).forall(_.trim.nonEmpty)

  def bucket(applicationConf: ApplicationConf): String = applicationConf.AhaWiki.aws.s3.bucket()

  def client(applicationConf: ApplicationConf): S3Client =
    clients.computeIfAbsent(clientKey(applicationConf), key => buildClient(key))

  def presigner(applicationConf: ApplicationConf): S3Presigner =
    presigners.computeIfAbsent(clientKey(applicationConf), key => buildPresigner(key))

  private def clientKey(applicationConf: ApplicationConf): ClientKey = {
    if (!isConfigured(applicationConf))
      throw new IllegalStateException(NotConfigured)
    ClientKey(
      applicationConf.AhaWiki.aws.AWS_REGION(),
      applicationConf.AhaWiki.aws.AWS_ACCESS_KEY_ID(),
      applicationConf.AhaWiki.aws.AWS_SECRET_ACCESS_KEY(),
    )
  }

  private def credentialsProvider(key: ClientKey): StaticCredentialsProvider =
    StaticCredentialsProvider.create(AwsBasicCredentials.create(key.accessKeyId, key.secretAccessKey))

  /** `endpoint` is for the specs, which stand a loopback server in for S3; the app never passes it. */
  private[logics] def buildClient(key: ClientKey, endpoint: Option[URI] = None): S3Client = {
    val builder = S3Client.builder()
      .region(Region.of(key.region))
      .credentialsProvider(credentialsProvider(key))
      .httpClientBuilder(Apache5HttpClient.builder())
    endpoint.foreach(uri => builder.endpointOverride(uri).forcePathStyle(true))
    builder.build()
  }

  private def buildPresigner(key: ClientKey): S3Presigner =
    S3Presigner.builder()
      .region(Region.of(key.region))
      .credentialsProvider(credentialsProvider(key))
      .build()

  /** Uploads a file and returns its ETag. See the other overload. */
  def putFile(applicationConf: ApplicationConf, objectKey: String, contentType: String, file: Path): Option[String] =
    putFile(client(applicationConf), bucket(applicationConf), objectKey, contentType, file)

  /**
   * Uploads a file and returns its ETag without the quotes S3 sends it in.
   *
   * SDK 1.x took the quotes off, so every ETag in the Attachment table was written without them;
   * SDK 2.x hands the header over as it is. The content type is set on the request because
   * `RequestBody.fromFile` would otherwise guess one from the file name, and an upload's file is a
   * temporary file whose name says nothing.
   */
  private[logics] def putFile(client: S3Client, bucket: String, objectKey: String, contentType: String, file: Path): Option[String] = {
    val request = PutObjectRequest.builder().bucket(bucket).key(objectKey).contentType(contentType).build()
    Option(client.putObject(request, RequestBody.fromFile(file)).eTag()).map(unquote)
  }

  private def unquote(eTag: String): String =
    if (eTag.length >= 2 && eTag.startsWith("\"") && eTag.endsWith("\"")) eTag.substring(1, eTag.length - 1) else eTag

  def deleteObject(applicationConf: ApplicationConf, objectKey: String): Unit =
    deleteObject(client(applicationConf), bucket(applicationConf), objectKey)

  private[logics] def deleteObject(client: S3Client, bucket: String, objectKey: String): Unit =
    client.deleteObject(DeleteObjectRequest.builder().bucket(bucket).key(objectKey).build())

  /**
   * What a batch delete did: the keys S3 says it deleted, and the ones it did not, each with S3's
   * code and message. No failures means every key is gone.
   *
   * `deleted` relies on the request not being quiet — a quiet delete answers with the failures
   * only. The admin browser marks the attachment rows of `deleted`, so it has to be the keys S3
   * confirmed, not the keys asked for.
   */
  case class DeleteObjectsResult(deleted: Seq[String], failures: Seq[String])

  /** Deletes the keys in one request. See the other overload. */
  def deleteObjects(applicationConf: ApplicationConf, objectKeys: Seq[String]): DeleteObjectsResult =
    deleteObjects(client(applicationConf), bucket(applicationConf), objectKeys)

  /**
   * Deletes the keys in one request.
   *
   * S3 answers a batch delete with 200 even when some keys failed, and lists those in the body.
   * SDK 1.x turned such a list into an exception; SDK 2.x returns it, so a caller that only
   * catches exceptions would report a partial delete as a complete one.
   */
  private[logics] def deleteObjects(client: S3Client, bucket: String, objectKeys: Seq[String]): DeleteObjectsResult = {
    val delete = Delete.builder().objects(objectKeys.map(key => ObjectIdentifier.builder().key(key).build()).asJava).build()
    val response = client.deleteObjects(DeleteObjectsRequest.builder().bucket(bucket).delete(delete).build())
    DeleteObjectsResult(
      deleted = response.deleted().asScala.toSeq.map(_.key()),
      failures = response.errors().asScala.toSeq.map(error => s"${error.key()}: ${error.code()} ${error.message()}"),
    )
  }
}
