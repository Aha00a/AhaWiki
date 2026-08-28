package com.aha00a.controllers

import com.aha00a.tests.TestApplication
import com.aha00a.tests.TestSchema
import org.scalatest.BeforeAndAfterAll
import org.scalatestplus.play.PlaySpec
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.Application
import play.api.cache.SyncCacheApi
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.test.FakeRequest
import play.api.test.Helpers._

import java.sql.DriverManager

/** Pins the one error envelope, `{"error": "<message>"}`, on the paths that used to answer
  * with something else. Dev ApiResponse records what each of these was and who was checked
  * before the change: every first-party consumer either throws on `!response.ok` without
  * reading the body, or already accepts both the old and the new shape. Envelope bodies had
  * no spec before this — status codes were guarded, shapes were not — which is exactly how
  * three-and-a-half shapes accumulated.
  */
class ApiErrorEnvelopeSpec extends PlaySpec with GuiceOneAppPerSuite with BeforeAndAfterAll {

  private val dbName = TestApplication.randomDbName("api_envelope")

  override def fakeApplication(): Application = {
    GuiceApplicationBuilder()
      .configure(TestApplication.baseConfiguration(dbName))
      .overrides(bind[SyncCacheApi].toInstance(new TestApplication.TestSyncCacheApi))
      .build()
  }

  override def beforeAll(): Unit = {
    super.beforeAll()
    Class.forName("org.h2.Driver")
    val connection = DriverManager.getConnection(TestApplication.h2Url(dbName), "sa", "")
    TestSchema.createAll()(connection)
    connection.close()
  }

  "AccessDenied" should {
    "answer an API route with the JSON error envelope" in {
      val result = route(app, FakeRequest(GET, "/api/Admin/Sites")).get
      status(result) mustBe FORBIDDEN
      contentType(result) mustBe Some(JSON)
      (contentAsJson(result) \ "error").as[String] mustBe "Access denied."
    }

    "answer an HTML admin route with the same envelope, deliberately" in {
      // A non-admin navigating to /Admin/Sites directly sees JSON instead of a plain
      // sentence. That is the decision, not an accident: the admin UI never links a
      // non-admin here, and one denial shape beats two.
      val result = route(app, FakeRequest(GET, "/Admin/Sites")).get
      status(result) mustBe FORBIDDEN
      contentType(result) mustBe Some(JSON)
      (contentAsJson(result) \ "error").as[String] mustBe "Access denied."
    }
  }

  "ApiCrawler.get" should {
    "reject an unsafe URL with the error envelope, not {\"message\"}" in {
      val result = route(app, FakeRequest(GET, "/api/crawler?q=http://127.0.0.1/")).get
      status(result) mustBe FORBIDDEN
      contentType(result) mustBe Some(JSON)
      (contentAsJson(result) \ "error").asOpt[String] mustBe defined
      (contentAsJson(result) \ "message").asOpt[String] mustBe None
    }
  }

  "Api.pageRevision" should {
    "answer a missing page with revision 0 and nothing else" in {
      val result = route(app, FakeRequest(GET, "/api/pageRevision/NoSuchPageForEnvelopeSpec")).get
      status(result) mustBe OK
      contentType(result) mustBe Some(JSON)
      (contentAsJson(result) \ "revision").as[Long] mustBe 0L
      (contentAsJson(result) \ "status").asOpt[String] mustBe None
    }
  }

  "Api.renderAhaMark" should {
    "reject a bodyless request with the error envelope" in {
      val result = route(app, FakeRequest(POST, "/api/renderAhaMark/AnyPage")).get
      status(result) mustBe BAD_REQUEST
      contentType(result) mustBe Some(JSON)
      (contentAsJson(result) \ "error").as[String] mustBe "JSON body is required"
    }
  }
}
