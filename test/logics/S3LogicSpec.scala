package logics

import com.sun.net.httpserver.HttpExchange
import com.sun.net.httpserver.HttpServer
import logics.wikis.macros.S3AttachmentUrlLogic
import org.scalatest.freespec.AnyFreeSpec
import play.api.Configuration
import software.amazon.awssdk.services.s3.S3Client
import software.amazon.awssdk.services.s3.model.ListObjectsV2Request

import java.net.InetAddress
import java.net.InetSocketAddress
import java.net.URI
import java.net.URLDecoder
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.util.concurrent.ConcurrentLinkedQueue
import scala.jdk.CollectionConverters._

/** What S3Logic and the presigned URL do, checked without reaching AWS -- the specs read an empty
  * AWS configuration, so nothing else in the suite gets this far.
  *
  * Signing is local, so a URL is signed here with made-up credentials and read back. The calls that
  * do go to S3 go to a loopback server standing in for it, through the client the app builds and
  * its Apache 5 transport. That checks the request that leaves the JVM and how the answer is read;
  * whether S3 itself accepts the request only the canary can show. */
class S3LogicSpec extends AnyFreeSpec {
  private val configured: Map[String, Any] = Map(
    "AhaWiki.aws.AWS_REGION" -> "eu-west-1",
    "AhaWiki.aws.AWS_ACCESS_KEY_ID" -> "test-access-key-id",
    "AhaWiki.aws.AWS_SECRET_ACCESS_KEY" -> "test-secret-access-key",
    "AhaWiki.aws.s3.bucket" -> "example-bucket",
  )

  private def conf(values: Map[String, Any]): ApplicationConf = new ApplicationConf(Configuration.from(values))

  private def presignedUrl(objectKey: String): URI =
    URI.create(S3AttachmentUrlLogic.generatePresignedUrl(conf(configured), objectKey).fold(e => fail(e), identity))

  private def queryOf(uri: URI): Map[String, String] =
    uri.getRawQuery.split('&').map(_.split("=", 2)).map(kv => kv(0) -> URLDecoder.decode(kv(1), StandardCharsets.UTF_8)).toMap

  private case class Received(method: String, rawPath: String, rawQuery: String, headers: Map[String, String], body: String)

  private case class Answer(status: Int, headers: Map[String, String] = Map.empty, body: String = "")

  /** Runs `test` against a loopback server that answers every request with `answer`, through a
    * client built the way the app builds one, and hands it what the server was sent. */
  private def withFakeS3[A](answer: Received => Answer)(test: (S3Client, () => Seq[Received]) => A): A = {
    val received = new ConcurrentLinkedQueue[Received]()
    val server = HttpServer.create(new InetSocketAddress(InetAddress.getLoopbackAddress, 0), 0)
    server.createContext("/", (exchange: HttpExchange) => {
      val request = Received(
        method = exchange.getRequestMethod,
        rawPath = exchange.getRequestURI.getRawPath,
        rawQuery = Option(exchange.getRequestURI.getRawQuery).getOrElse(""),
        headers = exchange.getRequestHeaders.asScala.map { case (k, v) => k.toLowerCase -> v.asScala.mkString(",") }.toMap,
        body = new String(exchange.getRequestBody.readAllBytes(), StandardCharsets.UTF_8),
      )
      received.add(request)
      val reply = answer(request)
      reply.headers.foreach { case (k, v) => exchange.getResponseHeaders.add(k, v) }
      val bytes = reply.body.getBytes(StandardCharsets.UTF_8)
      exchange.sendResponseHeaders(reply.status, if (bytes.isEmpty) -1 else bytes.length.toLong)
      if (bytes.nonEmpty) exchange.getResponseBody.write(bytes)
      exchange.close()
    })
    server.start()
    val endpoint = URI.create(s"http://${server.getAddress.getAddress.getHostAddress}:${server.getAddress.getPort}")
    val client = S3Logic.buildClient(S3Logic.ClientKey("eu-west-1", "test-access-key-id", "test-secret-access-key"), Some(endpoint))
    try test(client, () => received.asScala.toSeq)
    finally {
      client.close()
      server.stop(0)
    }
  }

  "isConfigured" - {
    "needs the region, both keys and the bucket, none of them blank" in {
      assert(S3Logic.isConfigured(conf(configured)))
      configured.keys.foreach { key =>
        assert(!S3Logic.isConfigured(conf(configured - key)), s"without $key")
        assert(!S3Logic.isConfigured(conf(configured + (key -> "  "))), s"with $key blank")
      }
    }
  }

  "while S3 is not configured" - {
    "no client or presigner is built" in {
      assert(intercept[IllegalStateException](S3Logic.client(conf(Map.empty))).getMessage === S3Logic.NotConfigured)
      assert(intercept[IllegalStateException](S3Logic.presigner(conf(Map.empty))).getMessage === S3Logic.NotConfigured)
    }

    "no URL is signed, and the reason says so" in {
      assert(S3AttachmentUrlLogic.generatePresignedUrl(conf(Map.empty), "Attachment/1/P/a.png") === Left(S3Logic.NotConfigured))
    }

    "a page has no attachments in S3" in {
      assert(AttachmentLogic.listPageObjectKeys(1, "P")(conf(Map.empty)) === Seq.empty)
    }
  }

  "client" - {
    // Building one reaches nothing, so this also says the SDK and its HTTP client load together.
    "is built once per credential set and then shared" in {
      assert(S3Logic.client(conf(configured)) eq S3Logic.client(conf(configured)))
    }
  }

  // Host and path are what SDK 1.x signed for the same keys on 2026-10-04. The query differs in two
  // ways: X-Amz-Credential and X-Amz-Expires trade places, and 1.x wrote 86399 for the day. (A bucket
  // name with dots would differ in the host as well; wiki page Dev Attachment has how.)
  "a presigned URL" - {
    "reads the object from the bucket's own host for a day" in {
      val url = presignedUrl("Attachment/1/대문 페이지/사진.png/사진.2026-10-04T12-00-00.png")
      assert(url.getScheme === "https")
      assert(url.getHost === "example-bucket.s3.eu-west-1.amazonaws.com")
      assert(url.getRawPath === "/Attachment/1/%EB%8C%80%EB%AC%B8%20%ED%8E%98%EC%9D%B4%EC%A7%80/%EC%82%AC%EC%A7%84.png/%EC%82%AC%EC%A7%84.2026-10-04T12-00-00.png")
      val query = queryOf(url)
      assert(query("X-Amz-Algorithm") === "AWS4-HMAC-SHA256")
      assert(query("X-Amz-SignedHeaders") === "host")
      assert(query("X-Amz-Expires") === "86400")
      assert(query("X-Amz-Credential").matches("test-access-key-id/\\d{8}/eu-west-1/s3/aws4_request"), query("X-Amz-Credential"))
      assert(query("X-Amz-Signature").matches("[0-9a-f]{64}"), query("X-Amz-Signature"))
    }

    // An upload's key is sanitized, but the admin browser signs whatever is in the bucket.
    "escapes what would otherwise read as part of the query" in {
      assert(presignedUrl("Attachment/1/A_B/a+b c&d=e?.png").getRawPath === "/Attachment/1/A_B/a%2Bb%20c%26d%3De%3F.png")
    }
  }

  "putFile" - {
    "sends the file with the content type it was given, and returns the ETag without quotes" in {
      // The name says text: a content type guessed from it would be text/plain.
      val file = Files.createTempFile("upload", ".txt")
      try {
        Files.write(file, "not really a png".getBytes(StandardCharsets.UTF_8))
        withFakeS3(_ => Answer(200, Map("ETag" -> "\"9b2cf535f27731c974343645a3985328\""))) { (client, received) =>
          val eTag = S3Logic.putFile(client, "example-bucket", "Attachment/1/페이지/a.png/a.2026-10-04T12-00-00.png", "image/png", file)
          assert(eTag === Some("9b2cf535f27731c974343645a3985328"))
          val Seq(put) = received()
          assert(put.method === "PUT")
          assert(put.rawPath === "/example-bucket/Attachment/1/%ED%8E%98%EC%9D%B4%EC%A7%80/a.png/a.2026-10-04T12-00-00.png")
          assert(put.headers.get("content-type") === Some("image/png"))
          assert(put.body.contains("not really a png"))
        }
      } finally Files.delete(file)
    }
  }

  "deleteObject" - {
    "deletes the one key" in {
      withFakeS3(_ => Answer(204)) { (client, received) =>
        S3Logic.deleteObject(client, "example-bucket", "Attachment/1/P/a b.png")
        val Seq(delete) = received()
        assert(delete.method === "DELETE")
        assert(delete.rawPath === "/example-bucket/Attachment/1/P/a%20b.png")
      }
    }
  }

  "deleteObjects" - {
    def deleteResult(inner: String): Answer =
      Answer(200, Map("Content-Type" -> "application/xml"),
        s"""<?xml version="1.0" encoding="UTF-8"?><DeleteResult xmlns="http://s3.amazonaws.com/doc/2006-03-01/">$inner</DeleteResult>""")

    // S3 answers 200 either way. SDK 1.x threw when the body listed failures; SDK 2.x does not.
    "returns the keys S3 did not delete, with its reason" in {
      withFakeS3(_ => deleteResult(
        "<Deleted><Key>Favicon/1/a.png</Key></Deleted>" +
          "<Error><Key>Favicon/1/b.png</Key><Code>AccessDenied</Code><Message>Access Denied</Message></Error>"
      )) { (client, received) =>
        assert(S3Logic.deleteObjects(client, "example-bucket", Seq("Favicon/1/a.png", "Favicon/1/b.png")) === Seq("Favicon/1/b.png: AccessDenied Access Denied"))
        val Seq(post) = received()
        assert(post.method === "POST")
        assert(post.rawQuery.split('&').contains("delete"))
        assert(post.body.contains("<Key>Favicon/1/a.png</Key>") && post.body.contains("<Key>Favicon/1/b.png</Key>"))
      }
    }

    "returns nothing when every key is gone" in {
      withFakeS3(_ => deleteResult("<Deleted><Key>Favicon/1/a.png</Key></Deleted>")) { (client, _) =>
        assert(S3Logic.deleteObjects(client, "example-bucket", Seq("Favicon/1/a.png")) === Seq.empty)
      }
    }
  }

  // The page delete and the attachment list read keys from here, and the admin browser shows them.
  // Neither SDK asks S3 to URL-encode the keys it lists (1.x sent encoding-type only when the request
  // set it, and nothing here does), so they arrive as XML text.
  "a listing" - {
    "reads back the keys as they are named" in {
      val listResult =
        """<?xml version="1.0" encoding="UTF-8"?><ListBucketResult xmlns="http://s3.amazonaws.com/doc/2006-03-01/">""" +
          "<Name>example-bucket</Name><Prefix>Attachment/1/</Prefix><KeyCount>1</KeyCount><MaxKeys>200</MaxKeys><IsTruncated>false</IsTruncated>" +
          "<Contents><Key>Attachment/1/페이지/a&amp;b c.png</Key><LastModified>2026-10-04T12:00:00.000Z</LastModified>" +
          "<ETag>&quot;9b2cf535f27731c974343645a3985328&quot;</ETag><Size>16</Size><StorageClass>STANDARD</StorageClass></Contents>" +
          "</ListBucketResult>"
      withFakeS3(_ => Answer(200, Map("Content-Type" -> "application/xml"), listResult)) { (client, received) =>
        val response = client.listObjectsV2(ListObjectsV2Request.builder().bucket("example-bucket").prefix("Attachment/1/").maxKeys(200).build())
        assert(response.contents().asScala.map(_.key()) === Seq("Attachment/1/페이지/a&b c.png"))
        val Seq(list) = received()
        assert(list.rawPath === "/example-bucket")
        assert(list.rawQuery.split('&').toSet.intersect(Set("list-type=2", "max-keys=200", "prefix=Attachment%2F1%2F")).size === 3)
      }
    }
  }
}
