package com.aha00a.controllers

import com.aha00a.tests.TestApplication
import controllers.WikiAttachment
import logics.AttachmentLogic
import org.apache.pekko.stream.Materializer
import org.apache.pekko.stream.scaladsl.Source
import org.apache.pekko.util.ByteString
import org.scalatestplus.play.PlaySpec
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.Application
import play.api.cache.SyncCacheApi
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.Files.TemporaryFile
import play.api.mvc.Action
import play.api.mvc.MultipartFormData
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers._

import scala.concurrent.Await
import scala.concurrent.duration._

/** How large a body the two upload routes take (AttachmentLogic.MaxUploadRequestBytes).
  *
  * Only the routes' body parsers run here, which is where the limit is enforced: a body over it is
  * answered with 413 before the action — and its permission check, S3 and the database — is reached.
  * Before 2026-11-18 the routes took Play's default of 10MB, and the 15MB case below got 413.
  *
  * The bodies are streamed in 1MiB chunks of zeros, so no case holds 50MB in memory. */
class WikiAttachmentUploadLimitSpec extends PlaySpec with GuiceOneAppPerSuite {

  override def fakeApplication(): Application =
    GuiceApplicationBuilder()
      .configure(TestApplication.baseConfiguration(TestApplication.randomDbName("uploadlimit")))
      .overrides(bind[SyncCacheApi].toInstance(new TestApplication.TestSyncCacheApi))
      .build()

  private val boundary = "AhaWikiUploadLimitSpecBoundary"
  private val head = ByteString(
    s"--$boundary\r\n" +
      "Content-Disposition: form-data; name=\"file\"; filename=\"big.png\"\r\n" +
      "Content-Type: image/png\r\n\r\n")
  private val tail = ByteString(s"\r\n--$boundary--\r\n")
  private val mebibyte = ByteString(new Array[Byte](1024 * 1024))

  private def fileBytes(size: Long): Source[ByteString, _] =
    Source.repeat(mebibyte).take(size / mebibyte.length) ++ Source.single(mebibyte.take((size % mebibyte.length).toInt))

  /** Runs the action's body parser over a multipart body holding one file of `size` bytes.
    * `withContentLength` sends the header a browser sends; without it the parser has to count. */
  private def parse(action: Action[MultipartFormData[TemporaryFile]], size: Long, withContentLength: Boolean = true): Either[Result, MultipartFormData[TemporaryFile]] = {
    implicit val materializer: Materializer = app.materializer
    val length = head.length + size + tail.length
    val headers = Seq(CONTENT_TYPE -> s"multipart/form-data; boundary=$boundary") ++
      (if (withContentLength) Seq(CONTENT_LENGTH -> length.toString) else Nil)
    val request = FakeRequest(POST, "/").withHeaders(headers: _*)
    Await.result(action.parser(request).run(Source.single(head) ++ fileBytes(size) ++ Source.single(tail)), 2.minutes)
  }

  private val mb15 = 15L * 1024 * 1024
  private val overhead = head.length + tail.length

  private def controller = app.injector.instanceOf[WikiAttachment]

  private val routes: Seq[(String, () => Action[MultipartFormData[TemporaryFile]])] = Seq(
    "uploadAttachment" -> (() => controller.uploadAttachment()),
    "uploadClipboardImage" -> (() => controller.uploadClipboardImage()),
  )

  routes.foreach { case (name, action) =>
    name should {
      "take a 15MB file, which Play's default limit refused" in {
        val parsed = parse(action(), mb15)
        parsed.isRight mustBe true
        parsed.toOption.get.file("file").map(_.fileSize) mustBe Some(mb15)
      }

      "take a body of exactly the limit" in {
        val size = AttachmentLogic.MaxUploadRequestBytes - overhead
        parse(action(), size).toOption.flatMap(_.file("file")).map(_.fileSize) mustBe Some(size)
      }

      "refuse a body one byte over the limit, by its Content-Length" in {
        val parsed = parse(action(), AttachmentLogic.MaxUploadRequestBytes - overhead + 1)
        parsed.left.toOption.map(_.header.status) mustBe Some(REQUEST_ENTITY_TOO_LARGE)
      }

      "refuse a body one byte over the limit, counting when there is no Content-Length" in {
        val parsed = parse(action(), AttachmentLogic.MaxUploadRequestBytes - overhead + 1, withContentLength = false)
        parsed.left.toOption.map(_.header.status) mustBe Some(REQUEST_ENTITY_TOO_LARGE)
      }
    }
  }
}
