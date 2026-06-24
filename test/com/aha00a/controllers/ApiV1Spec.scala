package com.aha00a.controllers

import anorm.SQL
import anorm.SqlStringInterpolation
import logics.AhaWikiCacheMemoryDomainSite
import logics.AhaWikiCacheMemoryPermission
import logics.SessionLogic
import models.WikiActors
import models.tables.Page
import models.tables.Site
import models.tables.UserApiKey
import org.apache.pekko.actor.ActorSystem
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

import java.time.LocalDateTime
import scala.collection.concurrent.TrieMap
import scala.concurrent.duration.Duration
import scala.reflect.ClassTag

class ApiV1Spec extends PlaySpec with GuiceOneAppPerSuite with BeforeAndAfterAll {

  private val dbName = s"bot_api_${java.util.UUID.randomUUID().toString.replace("-", "")}"
  private val actorSystem = ActorSystem(s"$dbName-actors")

  private class TestSyncCacheApi extends SyncCacheApi {
    private val values = TrieMap.empty[String, Any]

    override def set(key: String, value: Any, expiration: Duration): Unit =
      values.put(key, value)

    override def remove(key: String): Unit =
      values.remove(key)

    override def getOrElseUpdate[A](key: String, expiration: Duration)(orElse: => A)(implicit evidence$1: ClassTag[A]): A =
      values.getOrElseUpdate(key, orElse).asInstanceOf[A]

    override def get[T](key: String)(implicit evidence$2: ClassTag[T]): Option[T] =
      values.get(key).map(_.asInstanceOf[T])
  }

  override def fakeApplication(): Application = {
    GuiceApplicationBuilder()
      .configure(
        "db.default.driver"   -> "org.h2.Driver",
        "db.default.url"      -> s"jdbc:h2:mem:$dbName;MODE=MySQL;NON_KEYWORDS=USER;DB_CLOSE_DELAY=-1",
        "db.default.username" -> "sa",
        "db.default.password" -> "",
        "play.evolutions.db.default.enabled" -> false,
        "play.modules.disabled" -> Seq(
          "play.api.cache.redis.RedisCacheModule",
          "services.ApplicationLifecycleHook",
        ),
        "play.http.filters" -> "play.api.http.NoHttpFilters",
        "play.http.secret.key" -> "test-secret-key-for-testing-only",
        "AhaWiki.accessLog.sampleRate" -> 0,
      )
      .overrides(
        bind[SyncCacheApi].toInstance(new TestSyncCacheApi),
        bind[WikiActors].toInstance(WikiActors(actorSystem.deadLetters, actorSystem.deadLetters)),
      )
      .build()
  }

  private def db: Database = app.injector.instanceOf[Database]

  private implicit val site: Site = Site(1, "TestWiki", "TestWiki", "localhost")

  private def setupSchema(): Unit = {
    db.withConnection { implicit connection =>
      Seq(
        """
          CREATE TABLE Site (
            seq INT AUTO_INCREMENT PRIMARY KEY,
            created DATETIME DEFAULT NOW() NOT NULL,
            updated DATETIME DEFAULT NOW() NOT NULL,
            name VARCHAR(200) NOT NULL,
            abbr VARCHAR(200) NOT NULL DEFAULT '',
            mainDomain VARCHAR(255) NOT NULL DEFAULT '',
            publicListedOrder DECIMAL(10, 2) NULL
          )
        """,
        """
          CREATE TABLE SiteDomain (
            created DATETIME DEFAULT NOW() NOT NULL,
            site INT NOT NULL,
            domain VARCHAR(255) NOT NULL,
            PRIMARY KEY (site, domain)
          )
        """,
        """
          CREATE TABLE User (
            seq INT AUTO_INCREMENT PRIMARY KEY,
            created DATETIME DEFAULT NOW() NOT NULL,
            updated DATETIME DEFAULT NOW() NOT NULL,
            nickname VARCHAR(32) NOT NULL,
            profileImageUrl VARCHAR(255) NULL
          )
        """,
        """
          CREATE TABLE UserEmail (
            `user` INT NOT NULL,
            email VARCHAR(255) NOT NULL,
            isPrimary BOOLEAN NOT NULL DEFAULT FALSE,
            created DATETIME DEFAULT NOW() NOT NULL,
            PRIMARY KEY (`user`, email),
            UNIQUE (email)
          )
        """,
        """
          CREATE TABLE Permission (
            site INT NOT NULL,
            target VARCHAR(255) NOT NULL,
            targetType VARCHAR(255) NOT NULL,
            actor VARCHAR(255) NOT NULL,
            actorType VARCHAR(255) NOT NULL,
            action INT NOT NULL,
            dateUpdated DATETIME DEFAULT NOW() NOT NULL,
            PRIMARY KEY (site, target, targetType, actor, actorType)
          )
        """,
        """
          CREATE TABLE Page (
            site INT NOT NULL,
            name VARCHAR(255) NOT NULL,
            revision BIGINT NOT NULL,
            dateTime DATETIME DEFAULT NOW() NOT NULL,
            `user` INT NULL,
            remoteAddress VARCHAR(255) NOT NULL DEFAULT '',
            comment VARCHAR(255) NOT NULL DEFAULT '',
            isMinorEdit BOOLEAN NOT NULL DEFAULT FALSE,
            viaApi BOOLEAN NOT NULL DEFAULT FALSE,
            content CLOB NOT NULL,
            PRIMARY KEY (site, name, revision)
          )
        """,
        """
          CREATE TABLE PageMeta (
            site INT NOT NULL,
            name VARCHAR(255) NOT NULL,
            dateInserted DATETIME NOT NULL DEFAULT NOW(),
            dateUpdated DATETIME NULL,
            revision BIGINT NOT NULL,
            image VARCHAR(512) NULL,
            size BIGINT NOT NULL DEFAULT 0,
            PRIMARY KEY (site, name)
          )
        """,
        """
          CREATE TABLE CalculatedLink (
            site INT NOT NULL,
            src VARCHAR(255) NOT NULL,
            dst VARCHAR(255) NOT NULL,
            alias VARCHAR(255) NOT NULL DEFAULT ''
          )
        """,
        """
          CREATE TABLE CalculatedCosineSimilarity (
            site1 INT NOT NULL,
            name1 VARCHAR(255) NOT NULL,
            site2 INT NOT NULL,
            name2 VARCHAR(255) NOT NULL,
            similarity DOUBLE NOT NULL DEFAULT 0
          )
        """,
        """
          CREATE TABLE CalculatedTermFrequency (
            site INT NOT NULL,
            name VARCHAR(255) NOT NULL,
            term BIGINT NOT NULL,
            frequency INT NOT NULL DEFAULT 0
          )
        """,
        """
          CREATE TABLE CalculatedTermFrequencyNorm (
            site INT NOT NULL,
            name VARCHAR(255) NOT NULL,
            norm DOUBLE NOT NULL DEFAULT 0
          )
        """,
        """
          CREATE TABLE CalculatedSchemaOrg (
            site INT NOT NULL,
            page VARCHAR(255) NOT NULL,
            cls VARCHAR(255) NOT NULL DEFAULT '',
            prop VARCHAR(255) NOT NULL DEFAULT '',
            `value` VARCHAR(255) NOT NULL DEFAULT ''
          )
        """,
        """
          CREATE TABLE UserApiKey (
            seq BIGINT NOT NULL AUTO_INCREMENT,
            `user` INT NOT NULL,
            keyHash VARCHAR(64) NOT NULL,
            keyPrefix VARCHAR(32) NOT NULL,
            label VARCHAR(255) NOT NULL,
            dateInserted DATETIME NOT NULL DEFAULT NOW(),
            dateLastUsed DATETIME NULL,
            dateRevoked DATETIME NULL,
            PRIMARY KEY (seq),
            UNIQUE (keyHash)
          )
        """,
        """
          CREATE TABLE Attachment (
            seq BIGINT NOT NULL AUTO_INCREMENT,
            site BIGINT NOT NULL,
            pageName VARCHAR(255) NOT NULL,
            user BIGINT NULL,
            uploaderEmail VARCHAR(255) NULL,
            originalFilename VARCHAR(255) NOT NULL,
            storedFilename VARCHAR(255) NOT NULL,
            bucket VARCHAR(255) NOT NULL,
            objectKey VARCHAR(512) NOT NULL,
            contentType VARCHAR(255) NOT NULL,
            fileSize BIGINT NOT NULL,
            status VARCHAR(32) NOT NULL,
            etag VARCHAR(255) NULL,
            dateInserted DATETIME DEFAULT NOW() NOT NULL,
            dateUpdated DATETIME NULL,
            dateUploaded DATETIME NULL,
            dateDeleted DATETIME NULL,
            PRIMARY KEY (seq)
          )
        """,
        "CREATE INDEX UserApiKey_user_dateRevoked_index ON UserApiKey (`user`, dateRevoked)",
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
      UserApiKey.insert(1, "test key")
    }

  private def replaceLoginPermission(action: Int): Unit = {
    db.withConnection { implicit connection =>
      SQL"DELETE FROM Permission WHERE site = 1".executeUpdate()
      SQL"""
        INSERT INTO Permission (site, target, targetType, actor, actorType, action)
        VALUES (1, '', 'All', '', 'Login', $action)
      """.executeUpdate()
    }
    AhaWikiCacheMemoryPermission.clear()
  }

  private def insertPage(
    name: String,
    revision: Long,
    content: String,
    viaApi: Boolean = false,
    isMinorEdit: Boolean = false,
    dateTime: LocalDateTime = LocalDateTime.now(),
  ): Unit =
    db.withConnection { implicit connection =>
      Page.insert(Page(name, revision, dateTime, None, Some(1), "127.0.0.1", "", isMinorEdit, content, viaApi))
    }

  private def apiV1Request(method: String, url: String, key: String) =
    FakeRequest(method, url).withHeaders(HOST -> "localhost", AUTHORIZATION -> s"Bearer $key")

  private def loginRequest(method: String, url: String) =
    FakeRequest(method, url)
      .withHeaders(HOST -> "localhost")
      .withSession(
        SessionLogic.sessionKeySeq -> "1",
        SessionLogic.sessionKeyNickname -> "alice",
      )

  override def beforeAll(): Unit = {
    super.beforeAll()
    setupSchema()
    AhaWikiCacheMemoryDomainSite.invalidate()(db)
    AhaWikiCacheMemoryPermission.clear()
  }

  override def afterAll(): Unit = {
    actorSystem.terminate()
    super.afterAll()
  }

  "UserApiKey" should {
    "store only a SHA-256 hash and stop authenticating after revoke" in {
      val created = createApiKey()

      created.rawKey must startWith("ahawiki_")
      created.row.keyHash mustBe UserApiKey.hash(created.rawKey)
      created.row.keyHash must not include created.rawKey

      val authenticated = route(app, apiV1Request(GET, "/api/v1/page/Missing", created.rawKey)).get
      status(authenticated) mustBe NOT_FOUND

      db.withConnection { implicit connection =>
        UserApiKey.revoke(created.row.seq)
      }

      val revoked = route(app, apiV1Request(GET, "/api/v1/page/Missing", created.rawKey)).get
      status(revoked) mustBe UNAUTHORIZED

      val missing = route(app, apiV1Request(GET, "/api/v1/page/Missing", "ahawiki_missing")).get
      status(missing) mustBe UNAUTHORIZED
    }
  }

  "GET /api/v1/page/:name" should {
    "return page content for a valid API key user" in {
      val created = createApiKey()
      insertPage("ApiRead", 1, "= ApiRead\ncontent")

      val result = route(app, apiV1Request(GET, "/api/v1/page/ApiRead", created.rawKey)).get

      status(result) mustBe OK
      (contentAsJson(result) \ "name").as[String] mustBe "ApiRead"
      (contentAsJson(result) \ "revision").as[Long] mustBe 1
      (contentAsJson(result) \ "content").as[String] must include("content")
      (contentAsJson(result) \ "viaApi").as[Boolean] mustBe false
    }

    "return 403 when the API key user does not have read permission" in {
      val created = createApiKey()
      insertPage("ApiReadForbidden", 1, "= ApiReadForbidden\ncontent")

      try {
        replaceLoginPermission(0)

        val result = route(app, apiV1Request(GET, "/api/v1/page/ApiReadForbidden", created.rawKey)).get

        status(result) mustBe FORBIDDEN
      } finally {
        replaceLoginPermission(255)
      }
    }
  }

  "GET /api/v1/pages" should {
    "return readable page metadata with content hashes" in {
      val created = createApiKey()
      insertPage("ApiListA", 1, "= ApiListA\ncontent")
      insertPage("ApiListB", 1, "= ApiListB\ncontent")

      val result = route(app, apiV1Request(GET, "/api/v1/pages?prefix=ApiList", created.rawKey)).get

      status(result) mustBe OK
      val pages = (contentAsJson(result) \ "pages").as[Seq[play.api.libs.json.JsValue]]
      pages.map(page => (page \ "name").as[String]) must contain allOf("ApiListA", "ApiListB")
      pages.foreach { page =>
        (page \ "contentHash").as[String] must startWith("sha256:")
        (page \ "content").asOpt[String] mustBe None
        (page \ "revision").as[Long] mustBe 1
      }
    }
  }

  "POST /api/v1/pages/metadata" should {
    "return metadata for requested pages and report missing pages" in {
      val created = createApiKey()
      insertPage("ApiMetaA", 1, "= ApiMetaA\ncontent")

      val request = apiV1Request(POST, "/api/v1/pages/metadata", created.rawKey)
        .withJsonBody(Json.obj("names" -> Json.arr("ApiMetaA", "ApiMetaMissing")))

      val result = route(app, request).get

      status(result) mustBe OK
      val body = contentAsJson(result)
      val pages = (body \ "pages").as[Seq[play.api.libs.json.JsValue]]
      pages.map(page => (page \ "name").as[String]) mustBe Seq("ApiMetaA")
      (body \ "missing").as[Seq[String]] mustBe Seq("ApiMetaMissing")
    }
  }

  "GET /api/v1/changes" should {
    "return readable changes filtered by prefix and viaApi flag" in {
      val created = createApiKey()
      insertPage("ApiChangeWeb", 1, "= ApiChangeWeb\ncontent", viaApi = false)
      insertPage("ApiChangeViaApi", 1, "= ApiChangeViaApi\ncontent", viaApi = true)

      val result = route(app, apiV1Request(GET, "/api/v1/changes?prefix=ApiChange&includeViaApi=0", created.rawKey)).get

      status(result) mustBe OK
      val changes = (contentAsJson(result) \ "changes").as[Seq[play.api.libs.json.JsValue]]
      changes.map(change => (change \ "name").as[String]) must contain("ApiChangeWeb")
      changes.map(change => (change \ "name").as[String]) must not contain "ApiChangeViaApi"
    }

    "filter by since and minor edit flag" in {
      val created = createApiKey()
      insertPage("ApiChangeOld", 1, "= ApiChangeOld\ncontent", dateTime = LocalDateTime.parse("2026-06-24T09:00:00"))
      insertPage("ApiChangeMajor", 1, "= ApiChangeMajor\ncontent", dateTime = LocalDateTime.parse("2026-06-24T11:00:00"))
      insertPage("ApiChangeMinor", 1, "= ApiChangeMinor\ncontent", isMinorEdit = true, dateTime = LocalDateTime.parse("2026-06-24T12:00:00"))

      val result = route(app, apiV1Request(GET, "/api/v1/changes?prefix=ApiChange&since=2026-06-24T10:00:00&includeMinorEdit=0", created.rawKey)).get

      status(result) mustBe OK
      val names = (contentAsJson(result) \ "changes").as[Seq[play.api.libs.json.JsValue]].map(change => (change \ "name").as[String])
      names must contain("ApiChangeMajor")
      names must not contain "ApiChangeOld"
      names must not contain "ApiChangeMinor"
    }

    "reject afterRevision without an exact page name" in {
      val created = createApiKey()

      val result = route(app, apiV1Request(GET, "/api/v1/changes?prefix=ApiChange&afterRevision=1", created.rawKey)).get

      status(result) mustBe BAD_REQUEST
      (contentAsJson(result) \ "error").as[String] must include("afterRevision requires name")
    }

    "apply afterRevision only to an exact page name" in {
      val created = createApiKey()
      insertPage("ApiChangeOne", 1, "= ApiChangeOne\nold")
      insertPage("ApiChangeOne", 2, "= ApiChangeOne\nnew")
      insertPage("ApiChangeTwo", 1, "= ApiChangeTwo\ncontent")

      val result = route(app, apiV1Request(GET, "/api/v1/changes?name=ApiChangeOne&afterRevision=1", created.rawKey)).get

      status(result) mustBe OK
      val changes = (contentAsJson(result) \ "changes").as[Seq[play.api.libs.json.JsValue]]
      changes.map(change => (change \ "name").as[String]) mustBe Seq("ApiChangeOne")
      changes.map(change => (change \ "revision").as[Long]) mustBe Seq(2)
    }

    "reject invalid since values" in {
      val created = createApiKey()

      val result = route(app, apiV1Request(GET, "/api/v1/changes?since=not-a-date", created.rawKey)).get

      status(result) mustBe BAD_REQUEST
    }
  }

  "POST /api/v1/page/:name" should {
    "save a new revision with viaApi true" in {
      val created = createApiKey()
      insertPage("ApiSave", 1, "= ApiSave\nold")

      val request = apiV1Request(POST, "/api/v1/page/ApiSave", created.rawKey)
        .withJsonBody(Json.obj(
          "revision" -> 1,
          "text" -> "= ApiSave\nnew",
          "comment" -> "bot update",
          "minorEdit" -> true,
        ))

      val result = route(app, request).get

      status(result) mustBe OK
      (contentAsJson(result) \ "revision").as[Long] mustBe 2

      db.withConnection { implicit connection =>
        val page = Page.selectLastRevision("ApiSave").get
        page.revision mustBe 2
        page.content must include("new")
        page.comment mustBe "bot update"
        page.isMinorEdit mustBe true
        page.viaApi mustBe true
      }
    }

    "return 409 when request revision is stale" in {
      val created = createApiKey()
      insertPage("ApiConflict", 1, "= ApiConflict\nold")

      val request = apiV1Request(POST, "/api/v1/page/ApiConflict", created.rawKey)
        .withJsonBody(Json.obj(
          "revision" -> 0,
          "text" -> "= ApiConflict\nnew",
        ))

      val result = route(app, request).get

      status(result) mustBe CONFLICT
      (contentAsJson(result) \ "latestRevision").as[Long] mustBe 1
    }

    "return 403 when the API key user does not have write permission" in {
      val created = createApiKey()
      insertPage("ApiWriteForbidden", 1, "= ApiWriteForbidden\nold")

      try {
        replaceLoginPermission(1)

        val request = apiV1Request(POST, "/api/v1/page/ApiWriteForbidden", created.rawKey)
          .withJsonBody(Json.obj(
            "revision" -> 1,
            "text" -> "= ApiWriteForbidden\nnew",
          ))

        val result = route(app, request).get

        status(result) mustBe FORBIDDEN
      } finally {
        replaceLoginPermission(255)
      }
    }
  }

  "POST /api/v1/rename" should {
    "rename a page and create an API-marked redirect at the old name" in {
      val created = createApiKey()
      insertPage("ApiRename", 1, "= ApiRename\nold")

      val request = apiV1Request(POST, "/api/v1/rename", created.rawKey)
        .withJsonBody(Json.obj(
          "name" -> "ApiRename",
          "newName" -> "ApiRenamed",
          "revision" -> 1,
          "comment" -> "api rename",
        ))

      val result = route(app, request).get

      status(result) mustBe OK
      (contentAsJson(result) \ "newName").as[String] mustBe "ApiRenamed"

      db.withConnection { implicit connection =>
        val renamed = Page.selectLastRevision("ApiRenamed").get
        renamed.content must include("old")
        val redirect = Page.selectLastRevision("ApiRename").get
        redirect.content mustBe "#!redirect ApiRenamed"
        redirect.comment mustBe "api rename"
        redirect.viaApi mustBe true
      }
    }
  }

  "DELETE /api/v1/page/:name" should {
    "delete a page and mark related attachments deleted when revision and confirm are provided" in {
      val created = createApiKey()
      insertPage("ApiDelete", 1, "= ApiDelete\nold")
      db.withConnection { implicit connection =>
        SQL"""
          INSERT INTO Attachment
            (site, pageName, originalFilename, storedFilename, bucket, objectKey, contentType, fileSize, status)
          VALUES
            (1, 'ApiDelete', 'a.png', 'a.png', 'test-bucket', 'Attachment/1/ApiDelete/a.png', 'image/png', 10, 'Uploaded')
        """.executeUpdate()
      }

      val request = apiV1Request(DELETE, "/api/v1/page/ApiDelete", created.rawKey)
        .withJsonBody(Json.obj(
          "revision" -> 1,
          "confirm" -> true,
        ))

      val result = route(app, request).get

      status(result) mustBe OK
      (contentAsJson(result) \ "ok").as[Boolean] mustBe true

      db.withConnection { implicit connection =>
        Page.selectLastRevision("ApiDelete") mustBe None
        val deletedCount = SQL"SELECT COUNT(*) cnt FROM Attachment WHERE pageName = 'ApiDelete' AND status = 'Deleted' AND dateDeleted IS NOT NULL"
          .as(anorm.SqlParser.long("cnt").single)
        deletedCount mustBe 1
      }
    }

    "require explicit confirmation" in {
      val created = createApiKey()
      insertPage("ApiDeleteNoConfirm", 1, "= ApiDeleteNoConfirm\nold")

      val request = apiV1Request(DELETE, "/api/v1/page/ApiDeleteNoConfirm", created.rawKey)
        .withJsonBody(Json.obj("revision" -> 1))

      val result = route(app, request).get

      status(result) mustBe BAD_REQUEST
    }
  }

  "Account API key endpoints" should {
    "require login to create an API key" in {
      val result = route(app, FakeRequest(POST, "/api/account/ApiKeys").withHeaders(HOST -> "localhost").withJsonBody(Json.obj(
        "label" -> "anonymous key",
      ))).get

      status(result) mustBe UNAUTHORIZED
    }

    "create, list, and revoke the logged-in user's API key" in {
      val createResult = route(app, loginRequest(POST, "/api/account/ApiKeys").withJsonBody(Json.obj(
        "label" -> "account key",
      ))).get

      status(createResult) mustBe OK
      val createdJson = contentAsJson(createResult)
      val seq = (createdJson \ "seq").as[Long]
      val rawKey = (createdJson \ "key").as[String]
      (createdJson \ "label").as[String] mustBe "account key"

      val listResult = route(app, loginRequest(GET, "/api/account/ApiKeys")).get
      status(listResult) mustBe OK
      val keys = contentAsJson(listResult).as[Seq[play.api.libs.json.JsValue]]
      keys.exists(key => (key \ "seq").as[Long] == seq) mustBe true
      keys.find(key => (key \ "seq").as[Long] == seq).flatMap(key => (key \ "key").asOpt[String]) mustBe None

      val beforeRevoke = route(app, apiV1Request(GET, "/api/v1/page/Missing", rawKey)).get
      status(beforeRevoke) mustBe NOT_FOUND

      val revokeResult = route(app, loginRequest(DELETE, s"/api/account/ApiKeys/$seq")).get
      status(revokeResult) mustBe OK
      (contentAsJson(revokeResult) \ "ok").as[Boolean] mustBe true

      val afterRevoke = route(app, apiV1Request(GET, "/api/v1/page/Missing", rawKey)).get
      status(afterRevoke) mustBe UNAUTHORIZED
    }
  }
}
