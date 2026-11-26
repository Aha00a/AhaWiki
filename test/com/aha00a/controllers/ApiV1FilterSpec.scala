package com.aha00a.controllers

import anorm.SQL
import com.aha00a.tests.TestApplication
import com.aha00a.tests.TestSchema
import models.WikiActors
import models.tables.Page
import models.tables.Site
import models.tables.UserApiKey
import org.apache.pekko.actor.{ActorRef, ActorSystem}
import org.scalatest.BeforeAndAfterAll
import org.scalatestplus.play.PlaySpec
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.Application
import play.api.cache.SyncCacheApi
import play.api.db.Database
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.json.Json
import play.api.test.FakeRequest
import play.api.test.Helpers._


class ApiV1FilterSpec extends PlaySpec with GuiceOneAppPerSuite with BeforeAndAfterAll {

  private val dbName = TestApplication.randomDbName("api_v1_filter")
  private val actorSystem = ActorSystem(s"$dbName-actors")


  override def fakeApplication(): Application = {
    GuiceApplicationBuilder()
      .configure(TestApplication.baseConfiguration(dbName) ++ Map("play.http.filters" -> "Filters"))
      .overrides(
        bind[SyncCacheApi].toInstance(new TestApplication.TestSyncCacheApi),
        bind[WikiActors].toInstance(WikiActors(actorSystem.deadLetters, actorSystem.deadLetters)),
        bind[ActorRef].qualifiedWith("access-log-actor").toInstance(actorSystem.deadLetters),
      )
      .build()
  }

  private def db: Database = app.injector.instanceOf[Database]
  private implicit val site: Site = Site(1, "TestWiki", "TestWiki", "localhost")

  private def setupSchema(): Unit = {
    db.withConnection { implicit connection =>
      TestSchema.createAll()
      Seq(
        "INSERT INTO Site (seq, name, abbr, mainDomain) VALUES (1, 'TestWiki', 'TestWiki', 'localhost')",
        "INSERT INTO SiteDomain (site, domain) VALUES (1, 'localhost')",
        "INSERT INTO User (seq, nickname) VALUES (1, 'alice')",
        "INSERT INTO UserEmail (`user`, email, isPrimary) VALUES (1, 'alice@example.com', TRUE)",
        "INSERT INTO Permission (site, target, targetType, actor, actorType, action) VALUES (1, '', 'All', '', 'Login', 255)",
      ).foreach(sql => SQL(sql).execute())
    }
  }

  private def createApiKey(): UserApiKey.Created =
    db.withConnection { implicit connection =>
      UserApiKey.insert(1, "filter key")
    }

  override def beforeAll(): Unit = {
    super.beforeAll()
    setupSchema()
    TestApplication.resetMemoryCaches()
  }

  override def afterAll(): Unit = {
    actorSystem.terminate()
    super.afterAll()
  }

  "POST /api/v1/page/:name through Filters" should {
    "bypass CSRF and save with Bearer authentication only" in {
      val created = createApiKey()
      db.withConnection { implicit connection =>
        Page.insert(Page("ApiCsrf", 1, java.time.LocalDateTime.now(), None, Some(1), "127.0.0.1", "", isMinorEdit = false, "= ApiCsrf\nold"))
      }

      val result = route(app, FakeRequest(POST, "/api/v1/page/ApiCsrf")
        .withHeaders(HOST -> "localhost", AUTHORIZATION -> s"Bearer ${created.rawKey}")
        .withJsonBody(Json.obj(
          "revision" -> 1,
          "text" -> "= ApiCsrf\nnew",
        ))).get

      status(result) mustBe OK

      db.withConnection { implicit connection =>
        val page = Page.selectLastRevision("ApiCsrf").get
        page.revision mustBe 2
        page.viaApi mustBe true
      }
    }
  }

  // The answers before S3: a successful upload needs a bucket, which a spec does not have; that
  // path is the editor's own (WikiAttachment.storeFile) and was checked against the live wiki.
  "POST /api/v1/attachment/:name through Filters" should {
    import play.api.libs.Files.SingletonTemporaryFileCreator
    import play.api.mvc.MultipartFormData

    def body(withFile: Boolean): MultipartFormData[play.api.libs.Files.TemporaryFile] = {
      val file = SingletonTemporaryFileCreator.create("upload", ".png")
      java.nio.file.Files.write(file.path, Array[Byte](1, 2, 3))
      MultipartFormData(
        dataParts = Map.empty,
        files = if (withFile) Seq(MultipartFormData.FilePart("file", "a.png", Some("image/png"), file)) else Seq.empty,
        badParts = Seq.empty,
      )
    }
    def upload(page: String, key: Option[String], withFile: Boolean = true) =
      route(app, FakeRequest(POST, s"/api/v1/attachment/$page")
        .withHeaders((Seq(HOST -> "localhost") ++ key.map(k => AUTHORIZATION -> s"Bearer $k")): _*)
        .withMultipartFormDataBody(body(withFile))).get

    def setLoginPermission(action: Int): Unit = db.withConnection { implicit connection =>
      SQL(s"UPDATE Permission SET action = $action WHERE site = 1").executeUpdate()
      logics.AhaWikiCacheMemoryPermission.clear()
    }

    "answer 401 without a key -- not the CSRF filter's 403, which the editor's route gives" in {
      status(upload("ApiUpload", None)) mustBe UNAUTHORIZED
      val editorRoute = route(app, FakeRequest(POST, "/api/uploadAttachment")
        .withHeaders(HOST -> "localhost", AUTHORIZATION -> s"Bearer ${createApiKey().rawKey}")
        .withMultipartFormDataBody(body(withFile = true))).get
      status(editorRoute) mustBe FORBIDDEN
    }

    "answer 403 when the key's user may edit the page but not upload to it" in {
      val created = createApiKey()
      try {
        setLoginPermission(15) // Read, Edit, Create, Rename -- not Upload (16)
        val result = upload("ApiUpload", Some(created.rawKey))
        status(result) mustBe FORBIDDEN
        (contentAsJson(result) \ "error").asOpt[String] mustBe Some("Permission denied.")
      } finally {
        setLoginPermission(255)
      }
    }

    "answer 400 without a file part" in {
      val result = upload("ApiUpload", Some(createApiKey().rawKey), withFile = false)
      status(result) mustBe BAD_REQUEST
      (contentAsJson(result) \ "error").asOpt[String] mustBe Some("file is required.")
    }
  }
}
