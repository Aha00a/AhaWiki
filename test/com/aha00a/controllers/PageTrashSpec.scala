package com.aha00a.controllers

import anorm.SqlParser.long
import anorm.SqlParser.str
import anorm._
import com.aha00a.tests.TestApplication
import com.aha00a.tests.TestSchema
import logics.AttachmentLogic
import logics.SessionLogic
import models.WikiActors
import models.tables.Page
import models.tables.Site
import org.apache.pekko.actor.ActorSystem
import org.scalatest.BeforeAndAfterAll
import org.scalatestplus.play.PlaySpec
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.Application
import play.api.cache.SyncCacheApi
import play.api.db.Database
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.json.JsValue
import play.api.test.FakeRequest
import play.api.test.Helpers._

import java.time.LocalDateTime

/**
 * A deleted page is kept, and a site admin restores it or deletes it for good (PageTrashLogic,
 * evolution 75). Before 2026-11-16 a delete removed every revision and the attachments' objects.
 */
class PageTrashSpec extends PlaySpec with GuiceOneAppPerSuite with BeforeAndAfterAll {

  private val dbName = TestApplication.randomDbName("page_trash")
  private lazy val actorSystem = ActorSystem("page-trash-spec")

  override def fakeApplication(): Application =
    GuiceApplicationBuilder()
      .configure(TestApplication.baseConfiguration(dbName))
      .overrides(
        bind[SyncCacheApi].toInstance(new TestApplication.TestSyncCacheApi),
        bind[WikiActors].toInstance(WikiActors(actorSystem.deadLetters, actorSystem.deadLetters)),
      )
      .build()

  private def db: Database = app.injector.instanceOf[Database]

  private implicit val site: Site = Site(1, "TestWiki", "TestWiki", "localhost")

  override def beforeAll(): Unit = {
    super.beforeAll()
    db.withConnection { implicit connection =>
      TestSchema.createAll()
      Seq(
        "INSERT INTO Site (seq, name, abbr, mainDomain) VALUES (1, 'TestWiki', 'TestWiki', 'localhost')",
        "INSERT INTO SiteDomain (site, domain) VALUES (1, 'localhost')",
        "INSERT INTO User (seq, nickname) VALUES (1, 'owner')",
        "INSERT INTO User (seq, nickname) VALUES (2, 'bob')",
        "INSERT INTO UserEmail (`user`, email, isPrimary) VALUES (1, 'owner@example.com', TRUE)",
        "INSERT INTO UserEmail (`user`, email, isPrimary) VALUES (2, 'bob@example.com', TRUE)",
        "INSERT INTO Permission (site, target, targetType, actor, actorType, action) VALUES (1, '', 'All', '', 'Login', 255)",
      ).foreach(sql => SQL(sql).execute())
    }
    TestApplication.resetMemoryCaches()
  }

  override def afterAll(): Unit = {
    actorSystem.terminate()
    super.afterAll()
  }

  private def as(userSeq: Long, nickname: String)(method: String, url: String) =
    FakeRequest(method, url).withSession(SessionLogic.sessionKeySeq -> userSeq.toString, SessionLogic.sessionKeyNickname -> nickname)

  /** The owner is user 1, the super admin, and so a site admin of every site. */
  private def owner(method: String, url: String) = as(1, "owner")(method, url)

  private def insertPage(name: String, revision: Long, content: String): Unit =
    db.withConnection { implicit connection =>
      Page.insert(Page(name, revision, LocalDateTime.now(), None, Some(1), "127.0.0.1", s"r$revision", isMinorEdit = false, content))
    }

  private def insertAttachment(pageName: String, objectKey: String): Unit =
    db.withConnection { implicit connection =>
      SQL"""
        INSERT INTO Attachment (site, pageName, originalFilename, storedFilename, bucket, objectKey, contentType, fileSize, status)
        VALUES (1, $pageName, 'a.png', 'a.png', 'test-bucket', $objectKey, 'image/png', 10, 'Uploaded')
      """.executeUpdate()
    }

  private def attachment(objectKey: String): (String, Option[Long], Boolean) =
    db.withConnection { implicit connection =>
      SQL"SELECT status, deletedPage, dateDeleted FROM Attachment WHERE objectKey = $objectKey"
        .as((str("status") ~ SqlParser.get[Option[Long]]("deletedPage") ~ SqlParser.get[Option[LocalDateTime]]("dateDeleted")).map {
          case s ~ d ~ dd => (s, d, dd.isDefined)
        }.single)
    }

  private def keptSeq(name: String): Option[Long] =
    db.withConnection { implicit connection =>
      SQL"SELECT seq FROM DeletedPage WHERE site = 1 AND name = $name ORDER BY seq DESC".as(long("seq").*).headOption
    }

  private def count(sql: String): Long =
    db.withConnection { implicit connection => SQL(sql).as(long(1).single) }

  private def revisions(name: String): Seq[(Long, String)] =
    db.withConnection { implicit connection =>
      SQL"SELECT revision, content FROM Page WHERE site = 1 AND name = $name ORDER BY revision"
        .as((long("revision") ~ str("content")).map { case r ~ c => (r, c) }.*)
    }

  private def deleteViaWeb(name: String): Int =
    status(route(app, owner(POST, "/actions/delete").withFormUrlEncodedBody("name" -> name)).get)

  "Deleting a page" should {
    "keep every revision and its attachments instead of removing them" in {
      insertPage("Kept", 1, "first")
      insertPage("Kept", 2, "second")
      insertAttachment("Kept", "Attachment/1/Kept/a.png/a.1.png")

      deleteViaWeb("Kept") mustBe OK

      revisions("Kept") mustBe empty
      val seq = keptSeq("Kept").get
      count(s"SELECT COUNT(*) FROM DeletedPageRevision WHERE deletedPage = $seq") mustBe 2
      count(s"SELECT revisionCount FROM DeletedPage WHERE seq = $seq") mustBe 2
      count(s"SELECT lastRevision FROM DeletedPage WHERE seq = $seq") mustBe 2
      count(s"SELECT deletedBy FROM DeletedPage WHERE seq = $seq") mustBe 1
      attachment("Attachment/1/Kept/a.png/a.1.png") mustBe (("Trashed", Some(seq), true))
    }

    "keep a page whose only revision is deleted, since that deletes the page" in {
      insertPage("OneRevision", 1, "only")

      status(route(app, owner(POST, "/actions/deleteLastRevision").withFormUrlEncodedBody("name" -> "OneRevision")).get) mustBe OK

      revisions("OneRevision") mustBe empty
      keptSeq("OneRevision") mustBe defined
    }

    "not keep anything when the last of several revisions is deleted" in {
      insertPage("Undo", 1, "first")
      insertPage("Undo", 2, "second")

      status(route(app, owner(POST, "/actions/deleteLastRevision").withFormUrlEncodedBody("name" -> "Undo")).get) mustBe OK

      revisions("Undo") mustBe Seq(1L -> "first")
      keptSeq("Undo") mustBe None
    }

    "leave a kept page's objects alone when a new page of the same name lists them" in {
      insertPage("Again", 1, "old")
      insertAttachment("Again", "Attachment/1/Again/a.png/a.1.png")
      deleteViaWeb("Again") mustBe OK

      db.withConnection { implicit connection =>
        AttachmentLogic.heldElsewhere(1, "Again", Seq("Attachment/1/Again/a.png/a.1.png", "Attachment/1/Again/b.png/b.1.png")) mustBe
          Set("Attachment/1/Again/a.png/a.1.png")
      }
    }
  }

  "The deleted-page admin endpoints" should {
    "refuse a visitor who is not a site admin" in {
      status(route(app, FakeRequest(GET, "/api/Admin/Site/1/DeletedPages")).get) mustBe FORBIDDEN
      status(route(app, as(2, "bob")(GET, "/api/Admin/Site/1/DeletedPages")).get) mustBe FORBIDDEN
    }

    "list kept pages with their attachment count, and show the last revision's text" in {
      insertPage("Listed", 1, "one")
      insertPage("Listed", 2, "two")
      insertAttachment("Listed", "Attachment/1/Listed/a.png/a.1.png")
      deleteViaWeb("Listed") mustBe OK
      val seq = keptSeq("Listed").get

      val listed = contentAsJson(route(app, owner(GET, "/api/Admin/Site/1/DeletedPages")).get).as[Seq[JsValue]]
      val row = listed.find(r => (r \ "seq").as[Long] == seq).get
      (row \ "name").as[String] mustBe "Listed"
      (row \ "revisionCount").as[Long] mustBe 2
      (row \ "attachmentCount").as[Long] mustBe 1
      (row \ "deletedByNickname").as[String] mustBe "owner"

      val one = contentAsJson(route(app, owner(GET, s"/api/Admin/Site/1/DeletedPages/$seq")).get)
      (one \ "content").as[String] mustBe "two"
    }

    "restore every revision under the old name and give the attachments back" in {
      insertPage("Back", 1, "one")
      insertPage("Back", 2, "two")
      insertAttachment("Back", "Attachment/1/Back/a.png/a.1.png")
      deleteViaWeb("Back") mustBe OK
      val seq = keptSeq("Back").get

      val result = route(app, owner(POST, s"/api/Admin/Site/1/DeletedPages/$seq/restore")).get
      status(result) mustBe OK
      (contentAsJson(result) \ "name").as[String] mustBe "Back"

      revisions("Back") mustBe Seq(1L -> "one", 2L -> "two")
      attachment("Attachment/1/Back/a.png/a.1.png") mustBe (("Uploaded", None, false))
      keptSeq("Back") mustBe None
      count(s"SELECT COUNT(*) FROM DeletedPageRevision WHERE deletedPage = $seq") mustBe 0
    }

    "not restore over a page that took the name since" in {
      insertPage("Taken", 1, "old")
      deleteViaWeb("Taken") mustBe OK
      val seq = keptSeq("Taken").get
      insertPage("Taken", 1, "new")

      status(route(app, owner(POST, s"/api/Admin/Site/1/DeletedPages/$seq/restore")).get) mustBe CONFLICT

      revisions("Taken") mustBe Seq(1L -> "new")
      keptSeq("Taken") mustBe Some(seq)
    }

    "delete a kept page for good, attachments included" in {
      insertPage("Gone", 1, "bye")
      insertAttachment("Gone", "Attachment/1/Gone/a.png/a.1.png")
      deleteViaWeb("Gone") mustBe OK
      val seq = keptSeq("Gone").get

      status(route(app, owner(DELETE, s"/api/Admin/Site/1/DeletedPages/$seq")).get) mustBe OK

      keptSeq("Gone") mustBe None
      count(s"SELECT COUNT(*) FROM DeletedPageRevision WHERE deletedPage = $seq") mustBe 0
      attachment("Attachment/1/Gone/a.png/a.1.png") mustBe (("Deleted", None, true))
    }

    "answer 404 for a kept page of another site or none at all" in {
      status(route(app, owner(POST, "/api/Admin/Site/1/DeletedPages/999999/restore")).get) mustBe NOT_FOUND
      status(route(app, owner(DELETE, "/api/Admin/Site/1/DeletedPages/999999")).get) mustBe NOT_FOUND
    }
  }
}
