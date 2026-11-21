package com.aha00a.controllers

import anorm.SQL
import com.aha00a.tests.TestApplication
import com.aha00a.tests.TestSchema
import logics.SessionLogic
import org.scalatest.BeforeAndAfterAll
import org.scalatestplus.play.PlaySpec
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.Application
import play.api.cache.SyncCacheApi
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.json.Json
import play.api.routing.Router
import play.api.test.CSRFTokenHelper._
import play.api.test.FakeRequest
import play.api.test.Helpers._

/**
 * Every /api/Admin route refuses a caller who is not an admin -- read from the router, so a route
 * added tomorrow is checked without anyone remembering to add it here.
 *
 * Each action checks for itself (AdminAuth); nothing in front of them does it for all. Until
 * 2026-11-21 35 of the 48 routes had no test that would fail if its one line were dropped, and
 * some of them revoke API keys, delete S3 objects or change a site's settings.
 *
 * The refusal is told apart by its body, not its status. Specs run without HTTP filters
 * (TestApplication), so here every 403 is the action's; but in the running application the CSRF
 * filter also answers 403 to a form or multipart write without a token, and a status check would
 * then pass without reaching the action. The requests carry a token for the same reason.
 */
class AdminRoutesSpec extends PlaySpec with GuiceOneAppPerSuite with BeforeAndAfterAll {

  private val dbName = TestApplication.randomDbName("admin_routes")
  private val host = "admin-routes.test"

  override def fakeApplication(): Application = {
    GuiceApplicationBuilder()
      .configure(TestApplication.baseConfiguration(dbName))
      .overrides(bind[SyncCacheApi].toInstance(new TestApplication.TestSyncCacheApi))
      .build()
  }

  override def beforeAll(): Unit = {
    super.beforeAll()
    app.injector.instanceOf[play.api.db.Database].withConnection { implicit connection =>
      TestSchema.createAll()
      Seq(
        "INSERT INTO Site (seq, name, abbr, mainDomain) VALUES (1, 'AdminRoutes', 'AdminRoutes', 'admin-routes.test')",
        "INSERT INTO SiteDomain (site, domain) VALUES (1, 'admin-routes.test')",
        // Not user 1: that one is the instance admin (AdminLogic.SuperAdminUserSeq).
        "INSERT INTO `User` (seq, nickname) VALUES (41, 'plainuser')",
        "INSERT INTO UserEmail (`user`, email, isPrimary) VALUES (41, 'plain@example.com', TRUE)",
      ).foreach(sql => SQL(sql).execute())
    }
    TestApplication.resetMemoryCaches()
  }

  override def afterAll(): Unit = {
    TestApplication.resetMemoryCaches()
    super.afterAll()
  }

  // Each route as the router documents it: (method, path with its parameters filled in).
  private lazy val adminRoutes: Seq[(String, String)] =
    app.injector.instanceOf[Router].documentation
      .collect { case (method, path, _) if path.startsWith("/api/Admin/") => method -> path.replaceAll("""\$[A-Za-z]+<[^>]*>""", "1") }
      .distinct

  // What any of them may need in the query; an action reads what it wants and ignores the rest.
  private val query = "?seq=1&siteSeq=1&userSeq=1&key=x&url=https%3A%2F%2Fexample.com%2F&pageName=x&objectKey=x&name=x"

  private def call(method: String, path: String, asUser: Boolean) = {
    val base = FakeRequest(method, path + query).withHeaders(HOST -> host)
    val withUser = if (asUser) base.withSession(SessionLogic.sessionKeySeq -> "41", SessionLogic.sessionKeyNickname -> "plainuser") else base
    if (method == "GET" || method == "DELETE") route(app, withUser.withCSRFToken).get
    // The favicon upload parses multipart before the action runs, and answers 400 to anything else.
    else if (path.endsWith("/Favicon")) route(app, withUser.withMultipartFormDataBody(
      play.api.mvc.MultipartFormData[play.api.libs.Files.TemporaryFile](Map("siteSeq" -> Seq("1")), Seq.empty, Seq.empty)).withCSRFToken).get
    else route(app, withUser.withJsonBody(Json.obj()).withCSRFToken).get
  }

  // The admin's refusal: 403 {"error": "Access denied."}, or for the memory statistics the older
  // 401 {"error": ...}. Not the CSRF filter's 403, which has neither body.
  private def refusedByTheAction(method: String, path: String, asUser: Boolean): Option[String] = {
    val result = call(method, path, asUser)
    val code = status(result)
    val error = scala.util.Try((contentAsJson(result) \ "error").asOpt[String]).toOption.flatten
    val refused = (code == FORBIDDEN && error.contains("Access denied.")) || (code == UNAUTHORIZED && error.isDefined)
    if (refused) None else Some(s"$method $path -> $code ${contentAsString(result).take(80)}")
  }

  "every /api/Admin route" should {
    "be found in the router, so the checks below are not vacuous" in {
      adminRoutes.size must be >= 40
    }

    "refuse a caller with no session" in {
      adminRoutes.flatMap { case (m, p) => refusedByTheAction(m, p, asUser = false) } mustBe empty
    }

    "refuse a logged-in user who is neither the instance admin nor this site's admin" in {
      adminRoutes.flatMap { case (m, p) => refusedByTheAction(m, p, asUser = true) } mustBe empty
    }

  }
}
