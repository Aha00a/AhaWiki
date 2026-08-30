package com.aha00a.controllers

import anorm.SQL
import com.aha00a.tests.TestApplication
import com.aha00a.tests.TestSchema
import controllers.Api
import logics.SessionLogic
import org.scalatest.BeforeAndAfterAll
import org.scalatestplus.play.PlaySpec
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.Application
import play.api.cache.SyncCacheApi
import play.api.db.Database
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.inject.bind
import play.api.test.FakeRequest
import play.api.test.Helpers._

import java.sql.Connection
import java.sql.DriverManager

class ApiSiteAdminSpec extends PlaySpec with GuiceOneAppPerSuite with BeforeAndAfterAll {

  private val dbName = TestApplication.randomDbName("api_siteadmin")


  override def fakeApplication(): Application = {
    GuiceApplicationBuilder()
      .configure(TestApplication.baseConfiguration(dbName))
      .overrides(bind[SyncCacheApi].toInstance(new TestApplication.TestSyncCacheApi))
      .build()
  }

  private def db: Database = app.injector.instanceOf[Database]

  private def setupSchema(): Unit = {
    Class.forName("org.h2.Driver")
    val connection = DriverManager.getConnection(TestApplication.h2Url(dbName), "sa", "")
    TestSchema.createAll()(connection)
    Seq(
      "INSERT INTO Site (seq, name, abbr) VALUES (1, 'SiteA', 'SiteA')",
      "INSERT INTO `User` (seq, nickname) VALUES (1, 'superadmin')",
      "INSERT INTO `User` (seq, nickname) VALUES (10, 'alice')",
    ).foreach(sql => SQL(sql).execute()(connection))
    connection.close()
  }

  override def beforeAll(): Unit = {
    super.beforeAll()
    setupSchema()
    TestApplication.resetMemoryCaches()
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
      contentAsJson(result).as[Seq[play.api.libs.json.JsValue]] mustBe Seq.empty
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

  "GET /api/Admin/Site/:seq/Permissions" should {
    "return 403 for anonymous user" in {
      val result = route(app, anonymousRequest(GET, "/api/Admin/Site/1/Permissions")).get
      status(result) mustBe FORBIDDEN
    }

    // The admin screen fills its three dropdowns from this response. It used to carry its own
    // copy of these lists, and the copy drifted — no Rename at all, Upload at 8 and Delete at
    // 16 — so the screen offered numbers its own row list contradicted.
    "serve the permission vocabulary the screen fills its dropdowns from" in {
      val result = route(app, adminRequest(GET, "/api/Admin/Site/1/Permissions")).get
      status(result) mustBe OK

      val json = contentAsJson(result)
      (json \ "targetTypes").as[Seq[String]] mustBe Seq("All", "Exact", "StartsWith", "EndsWith", "RegularExpression")
      (json \ "actorTypes").as[Seq[String]] mustBe Seq("All", "Login", "Exact", "Domain")

      val actions = (json \ "actions").as[Seq[play.api.libs.json.JsValue]]
        .map(a => ((a \ "name").as[String], (a \ "action").as[Int]))
      actions mustBe Seq(
        ("None", 0), ("Read", 1), ("Edit", 2), ("Create", 4),
        ("Rename", 8), ("Upload", 16), ("Delete", 32), ("Admin", 255),
      )
    }
  }

  "DELETE /api/Admin/Site/:seq/Admins/:userSeq" should {
    "return 403 for anonymous user" in {
      val result = route(app, anonymousRequest(DELETE, "/api/Admin/Site/1/Admins/10")).get
      status(result) mustBe FORBIDDEN
    }

    "delete site admin and return 200" in {
      db.withConnection { implicit c =>
        models.tables.SiteAdmin.delete(1, 10)
        models.tables.SiteAdmin.insert(1, 10)
      }
      val result = route(app, adminRequest(DELETE, "/api/Admin/Site/1/Admins/10")).get
      status(result) mustBe OK
      (contentAsJson(result) \ "ok").as[Boolean] mustBe true
    }
  }
}
