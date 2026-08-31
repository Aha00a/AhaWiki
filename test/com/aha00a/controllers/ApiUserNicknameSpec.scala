package com.aha00a.controllers

import anorm.SQL
import com.aha00a.tests.TestApplication
import com.aha00a.tests.TestSchema
import logics.AdminLogic
import logics.SessionLogic
import models.tables.User
import models.tables.UserNicknameHistory
import org.scalatest.BeforeAndAfterAll
import org.scalatestplus.play.PlaySpec
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.Application
import play.api.cache.SyncCacheApi
import play.api.db.Database
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.test.FakeRequest
import play.api.test.Helpers._

import java.sql.DriverManager

/**
 * A user asks for a nickname; an admin answers. The parts worth pinning are the ones where
 * being wrong is quiet: a request that renames somebody without approval, an approval that
 * fires twice, or an account page that believes the cookie over the database.
 */
class ApiUserNicknameSpec extends PlaySpec with GuiceOneAppPerSuite with BeforeAndAfterAll {

  private val dbName = TestApplication.randomDbName("api_usernickname")

  private val aliceSeq = 10L
  private val bobSeq = 20L

  override def fakeApplication(): Application = {
    GuiceApplicationBuilder()
      .configure(TestApplication.baseConfiguration(dbName))
      .overrides(bind[SyncCacheApi].toInstance(new TestApplication.TestSyncCacheApi))
      .build()
  }

  private def db: Database = app.injector.instanceOf[Database]

  override def beforeAll(): Unit = {
    super.beforeAll()
    Class.forName("org.h2.Driver")
    val connection = DriverManager.getConnection(TestApplication.h2Url(dbName), "sa", "")
    TestSchema.createAll()(connection)
    Seq(
      "INSERT INTO Site (seq, name, abbr) VALUES (1, 'SiteA', 'SiteA')",
      s"INSERT INTO `User` (seq, nickname) VALUES (${AdminLogic.SuperAdminUserSeq}, 'superadmin')",
      s"INSERT INTO `User` (seq, nickname) VALUES ($aliceSeq, 'alicewiki')",
      s"INSERT INTO `User` (seq, nickname) VALUES ($bobSeq, 'bobwiki')",
    ).foreach(sql => SQL(sql).execute()(connection))
    connection.close()
    TestApplication.resetMemoryCaches()
  }

  private def session(seq: Long, nickname: String) = Seq(
    SessionLogic.sessionKeySeq -> seq.toString,
    SessionLogic.sessionKeyNickname -> nickname,
  )

  private def as(seq: Long, nickname: String)(method: String, url: String) =
    FakeRequest(method, url).withSession(session(seq, nickname): _*)

  private def anonymous(method: String, url: String) = FakeRequest(method, url)

  private def requestNickname(seq: Long, nickname: String, wanted: String) =
    route(app, as(seq, nickname)(POST, "/api/account/NicknameRequests").withFormUrlEncodedBody("nickname" -> wanted)).get

  private def nicknameOf(seq: Long): String =
    db.withConnection(implicit c => User.selectBySeq(seq).map(_.nickname).getOrElse(""))

  private def resetState(): Unit = db.withConnection { implicit c =>
    SQL("DELETE FROM UserNicknameChangeRequest").executeUpdate()
    SQL("DELETE FROM UserNicknameHistory").executeUpdate()
    SQL(s"UPDATE `User` SET nickname = 'alicewiki' WHERE seq = $aliceSeq").executeUpdate()
    SQL(s"UPDATE `User` SET nickname = 'bobwiki' WHERE seq = $bobSeq").executeUpdate()
  }

  private def newPendingRequest(): Long = {
    val result = requestNickname(aliceSeq, "alicewiki", "새이름한글")
    status(result) mustBe OK
    (contentAsJson(result) \ "seq").as[Long]
  }

  "the account endpoints" should {
    "refuse anonymous callers" in {
      status(route(app, anonymous(GET, "/api/account/NicknameRequests")).get) mustBe UNAUTHORIZED
      status(route(app, anonymous(POST, "/api/account/NicknameRequests")).get) mustBe UNAUTHORIZED
    }

    "report the nickname the database holds, not the one the cookie remembers" in {
      resetState()
      // A session signed before some earlier rename. Identity is the seq; the name is a
      // display snapshot, and the account page is exactly where believing it would mislead.
      val result = route(app, as(aliceSeq, "a-name-from-last-week")(GET, "/api/account/NicknameRequests")).get

      status(result) mustBe OK
      (contentAsJson(result) \ "currentNickname").as[String] mustBe "alicewiki"
    }

    "create a pending request" in {
      resetState()
      val result = requestNickname(aliceSeq, "alicewiki", "새이름한글")

      status(result) mustBe OK
      (contentAsJson(result) \ "status").as[String] mustBe "Pending"
      (contentAsJson(result) \ "requestedNickname").as[String] mustBe "새이름한글"
      nicknameOf(aliceSeq) mustBe "alicewiki"
    }

    "answer with the validation rule that was broken" in {
      resetState()
      val result = requestNickname(aliceSeq, "alicewiki", "abc")

      status(result) mustBe BAD_REQUEST
      (contentAsJson(result) \ "error").as[String] must include("at least 5")
    }

    "refuse a nickname somebody else already has" in {
      resetState()
      val result = requestNickname(aliceSeq, "alicewiki", "bobwiki")

      status(result) mustBe BAD_REQUEST
      (contentAsJson(result) \ "error").as[String] must include("already in use")
    }

    "refuse the nickname the requester already has" in {
      resetState()
      val result = requestNickname(aliceSeq, "alicewiki", "alicewiki")

      status(result) mustBe BAD_REQUEST
      (contentAsJson(result) \ "error").as[String] must include("already your nickname")
    }

    "allow only one pending request per user" in {
      resetState()
      newPendingRequest()
      // Five characters or more, or the length rule answers first — validation runs before
      // the pending check on purpose, so that a bad name is not told it is merely queued.
      val second = requestNickname(aliceSeq, "alicewiki", "다른이름다섯")

      status(second) mustBe BAD_REQUEST
      (contentAsJson(second) \ "error").as[String] must include("waiting for review")
    }

    "refuse a nickname somebody else is already waiting on" in {
      resetState()
      newPendingRequest()
      val other = requestNickname(bobSeq, "bobwiki", "새이름한글")

      status(other) mustBe BAD_REQUEST
      (contentAsJson(other) \ "error").as[String] must include("already requested")
    }

    "let the requester withdraw, and ask again afterwards" in {
      resetState()
      val seq = newPendingRequest()

      val canceled = route(app, as(aliceSeq, "alicewiki")(POST, s"/api/account/NicknameRequests/$seq/cancel")).get
      status(canceled) mustBe OK
      (contentAsJson(canceled) \ "status").as[String] mustBe "Canceled"

      status(requestNickname(aliceSeq, "alicewiki", "또다른이름")) mustBe OK
    }

    "refuse to withdraw somebody else's request" in {
      resetState()
      val seq = newPendingRequest()

      val result = route(app, as(bobSeq, "bobwiki")(POST, s"/api/account/NicknameRequests/$seq/cancel")).get
      status(result) mustBe NOT_FOUND
      nicknameOf(aliceSeq) mustBe "alicewiki"
    }
  }

  "the admin endpoints" should {
    "refuse anyone who is not an admin" in {
      resetState()
      val seq = newPendingRequest()

      status(route(app, as(aliceSeq, "alicewiki")(GET, "/api/Admin/NicknameRequests")).get) mustBe FORBIDDEN
      status(route(app, as(aliceSeq, "alicewiki")(POST, s"/api/Admin/NicknameRequests/$seq/approve")).get) mustBe FORBIDDEN
      status(route(app, as(aliceSeq, "alicewiki")(POST, s"/api/Admin/NicknameRequests/$seq/reject")).get) mustBe FORBIDDEN
      nicknameOf(aliceSeq) mustBe "alicewiki"
    }

    "list pending requests with the requester's current nickname" in {
      resetState()
      newPendingRequest()

      val result = route(app, as(AdminLogic.SuperAdminUserSeq, "superadmin")(GET, "/api/Admin/NicknameRequests")).get
      status(result) mustBe OK
      val rows = contentAsJson(result).as[Seq[play.api.libs.json.JsValue]]
      rows.map(r => (r \ "userNickname").as[String]) mustBe Seq("alicewiki")
      rows.map(r => (r \ "requestedNickname").as[String]) mustBe Seq("새이름한글")
    }

    "rename the user on approval, and record what it used to be" in {
      resetState()
      val seq = newPendingRequest()

      val result = route(app, as(AdminLogic.SuperAdminUserSeq, "superadmin")(POST, s"/api/Admin/NicknameRequests/$seq/approve")).get
      status(result) mustBe OK
      (contentAsJson(result) \ "status").as[String] mustBe "Approved"
      nicknameOf(aliceSeq) mustBe "새이름한글"

      val history = db.withConnection(implicit c => UserNicknameHistory.selectByUser(aliceSeq))
      history.map(h => (h.old, h.updated, h.changedBy)) mustBe Seq(("alicewiki", "새이름한글", AdminLogic.SuperAdminUserSeq))
    }

    "approve once, however many times it is clicked" in {
      resetState()
      val seq = newPendingRequest()
      val admin = as(AdminLogic.SuperAdminUserSeq, "superadmin")(POST, s"/api/Admin/NicknameRequests/$seq/approve")

      status(route(app, admin).get) mustBe OK
      val second = route(app, admin).get

      status(second) mustBe BAD_REQUEST
      (contentAsJson(second) \ "error").as[String] must include("no longer pending")
      // One rename, so one history row — a second would claim a change from a name the user
      // never had by then.
      db.withConnection(implicit c => UserNicknameHistory.selectByUser(aliceSeq)).size mustBe 1
    }

    "refuse to approve a nickname taken while the request waited" in {
      resetState()
      val seq = newPendingRequest()
      db.withConnection(implicit c => User.updateNickname(bobSeq, "새이름한글"))

      val result = route(app, as(AdminLogic.SuperAdminUserSeq, "superadmin")(POST, s"/api/Admin/NicknameRequests/$seq/approve")).get

      status(result) mustBe BAD_REQUEST
      (contentAsJson(result) \ "error").as[String] must include("already in use")
      nicknameOf(aliceSeq) mustBe "alicewiki"
      db.withConnection(implicit c => UserNicknameHistory.selectByUser(aliceSeq)) mustBe empty
    }

    "keep the nickname on rejection, and store the reason" in {
      resetState()
      val seq = newPendingRequest()

      val result = route(app, as(AdminLogic.SuperAdminUserSeq, "superadmin")(POST, s"/api/Admin/NicknameRequests/$seq/reject")
        .withFormUrlEncodedBody("rejectReason" -> "이미 쓰는 사람이 있는 이름과 너무 비슷합니다")).get

      status(result) mustBe OK
      (contentAsJson(result) \ "status").as[String] mustBe "Rejected"
      (contentAsJson(result) \ "rejectReason").as[String] mustBe "이미 쓰는 사람이 있는 이름과 너무 비슷합니다"
      nicknameOf(aliceSeq) mustBe "alicewiki"
      db.withConnection(implicit c => UserNicknameHistory.selectByUser(aliceSeq)) mustBe empty
    }

    "answer 404 for a request that does not exist" in {
      resetState()
      status(route(app, as(AdminLogic.SuperAdminUserSeq, "superadmin")(POST, "/api/Admin/NicknameRequests/99999/approve")).get) mustBe NOT_FOUND
    }
  }
}
