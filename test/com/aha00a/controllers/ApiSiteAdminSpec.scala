package com.aha00a.controllers

import anorm.SQL
import controllers.Api
import logics.SessionLogic
import org.scalatest.BeforeAndAfterAll
import org.scalatestplus.play.PlaySpec
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.Application
import play.api.db.Database
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.test.FakeRequest
import play.api.test.Helpers._

import java.sql.Connection
import java.sql.DriverManager

class ApiSiteAdminSpec extends PlaySpec with GuiceOneAppPerSuite with BeforeAndAfterAll {

  private val dbName = s"api_siteadmin_${java.util.UUID.randomUUID().toString.replace("-", "")}"

  override def fakeApplication(): Application = {
    GuiceApplicationBuilder()
      .configure(
        "db.default.driver"   -> "org.h2.Driver",
        "db.default.url"      -> s"jdbc:h2:mem:$dbName;MODE=MySQL;DB_CLOSE_DELAY=-1",
        "db.default.username" -> "sa",
        "db.default.password" -> "",
        "play.evolutions.db.default.enabled" -> false,
        "play.modules.disabled" -> Seq(
          "play.api.cache.redis.RedisCacheModule",
          "services.ApplicationLifecycleHook",
        ),
        "play.http.secret.key" -> "test-secret-key-for-testing-only",
        "AhaWiki.accessLog.sampleRate" -> 0,
      )
      .build()
  }

  private def db: Database = app.injector.instanceOf[Database]

  private def setupSchema(): Unit = {
    Class.forName("org.h2.Driver")
    val connection = DriverManager.getConnection(s"jdbc:h2:mem:$dbName;MODE=MySQL;DB_CLOSE_DELAY=-1", "sa", "")
    Seq(
      """
        CREATE TABLE IF NOT EXISTS Site (
          seq INT AUTO_INCREMENT PRIMARY KEY,
          created DATETIME DEFAULT NOW() NOT NULL,
          updated DATETIME DEFAULT NOW() NOT NULL,
          name VARCHAR(200) NOT NULL
        )
      """,
      """
        CREATE TABLE IF NOT EXISTS User (
          seq INT AUTO_INCREMENT PRIMARY KEY,
          created DATETIME DEFAULT NOW() NOT NULL,
          updated DATETIME DEFAULT NOW() NOT NULL,
          nickname VARCHAR(32) NOT NULL
        )
      """,
      """
        CREATE TABLE IF NOT EXISTS SiteAdmin (
          site INT NOT NULL,
          `user` INT NOT NULL,
          dateInserted DATETIME DEFAULT NOW() NOT NULL,
          PRIMARY KEY (site, `user`),
          CONSTRAINT SiteAdmin_Site_seq_fk FOREIGN KEY (site) REFERENCES Site (seq),
          CONSTRAINT SiteAdmin_User_seq_fk FOREIGN KEY (`user`) REFERENCES User (seq)
        )
      """,
      "INSERT INTO Site (seq, name) VALUES (1, 'SiteA')",
      "INSERT INTO User (seq, nickname) VALUES (1, 'superadmin')",
      "INSERT INTO User (seq, nickname) VALUES (10, 'alice')",
    ).foreach(sql => SQL(sql).execute()(connection))
    connection.close()
  }

  override def beforeAll(): Unit = {
    super.beforeAll()
    setupSchema()
  }

  private def superAdminSession = Seq(
    SessionLogic.sessionKeySeq      -> "1",
    SessionLogic.sessionKeyNickname -> "superadmin",
  )

  private def anonymousRequest(method: String, url: String) =
    FakeRequest(method, url)

  private def adminRequest(method: String, url: String) =
    FakeRequest(method, url).withSession(superAdminSession: _*)

  "GET /api/Admin/Site/:seq/Admins" should {
    "return 403 for anonymous user" in {
      val result = route(app, anonymousRequest(GET, "/api/Admin/Site/1/Admins")).get
      status(result) mustBe FORBIDDEN
    }

    "return 200 and empty array for super admin when no site admins" in {
      val result = route(app, adminRequest(GET, "/api/Admin/Site/1/Admins")).get
      status(result) mustBe OK
      contentAsString(result) mustBe "[]"
    }

    "return 404 for non-existent site" in {
      val result = route(app, adminRequest(GET, "/api/Admin/Site/999/Admins")).get
      status(result) mustBe NOT_FOUND
    }
  }

  "POST /api/Admin/Site/:seq/Admins" should {
    "return 403 for anonymous user" in {
      val result = route(app, anonymousRequest(POST, "/api/Admin/Site/1/Admins")).get
      status(result) mustBe FORBIDDEN
    }

    "return 400 when user param is missing" in {
      val result = route(app, adminRequest(POST, "/api/Admin/Site/1/Admins")
        .withFormUrlEncodedBody()).get
      status(result) mustBe BAD_REQUEST
    }

    "insert site admin and return 200" in {
      val result = route(app, adminRequest(POST, "/api/Admin/Site/1/Admins")
        .withFormUrlEncodedBody("user" -> "10")).get
      status(result) mustBe OK
      (contentAsJson(result) \ "ok").as[Boolean] mustBe true

      val listResult = route(app, adminRequest(GET, "/api/Admin/Site/1/Admins")).get
      status(listResult) mustBe OK
      val admins = contentAsJson(listResult).as[Seq[play.api.libs.json.JsValue]]
      admins.map(a => (a \ "user").as[Long]) must contain(10L)
    }
  }

  "DELETE /api/Admin/Site/:seq/Admins/:userSeq" should {
    "return 403 for anonymous user" in {
      val result = route(app, anonymousRequest(DELETE, "/api/Admin/Site/1/Admins/10")).get
      status(result) mustBe FORBIDDEN
    }

    "delete site admin and return 200" in {
      db.withConnection { implicit c =>
        models.tables.SiteAdmin.insert(1, 10)
      }
      val result = route(app, adminRequest(DELETE, "/api/Admin/Site/1/Admins/10")).get
      status(result) mustBe OK
      (contentAsJson(result) \ "ok").as[Boolean] mustBe true
    }
  }
}
