package com.aha00a.controllers

import anorm.SQL
import com.aha00a.tests.TestApplication
import com.aha00a.tests.TestSchema
import logics.SessionLogic
import models.WikiActors
import org.apache.pekko.actor.ActorSystem
import org.scalatest.BeforeAndAfterAll
import org.scalatestplus.play.PlaySpec
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.Application
import play.api.cache.SyncCacheApi
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.test.FakeRequest
import play.api.test.Helpers._

/** A page opened from its history, at `?revision=N`.
  *
  * The content was always revision N, but everything around it described the latest: the
  * revisionInfo box beside it, and on a Kanban page the board itself, which was redrawn as the
  * latest board the next time anyone saved the page. Until 2026-09-26 an old revision therefore
  * looked like the current page. Now the view names the revision it shows, is read-only, and does
  * not open the page's WebSocket. The wiki page Dev Page has the reasons.
  *
  * `AHAWIKI_RENDER_DUMP` writes the old revision's HTML there, as AccountSettingsRenderSpec does,
  * so the notice can be looked at without a database to run the real server against.
  *
  * The site has a seq and host of its own, because the memory caches are shared by every spec in
  * the JVM and keyed by site. */
class WikiOldRevisionViewSpec extends PlaySpec with GuiceOneAppPerSuite with BeforeAndAfterAll {

  private val dbName = TestApplication.randomDbName("old_revision")
  private val host = "old-revision.test"
  private val actorSystem = ActorSystem(s"$dbName-actors")

  override def fakeApplication(): Application =
    GuiceApplicationBuilder()
      .configure(TestApplication.baseConfiguration(dbName))
      .overrides(
        bind[SyncCacheApi].toInstance(new TestApplication.TestSyncCacheApi),
        // A view now and then sends its page off to be recalculated; nothing here needs that done.
        bind[WikiActors].toInstance(WikiActors(actorSystem.deadLetters, actorSystem.deadLetters)),
      )
      .build()

  override def beforeAll(): Unit = {
    super.beforeAll()
    app.injector.instanceOf[play.api.db.Database].withConnection { implicit connection =>
      TestSchema.createAll()
      Seq(
        "INSERT INTO Site (seq, name, abbr, mainDomain) VALUES (58, 'OldRevision', 'OldRevision', 'old-revision.test')",
        "INSERT INTO SiteDomain (site, domain) VALUES (58, 'old-revision.test')",
        "INSERT INTO `User` (seq, nickname) VALUES (61, 'editor')",
        "INSERT INTO UserEmail (`user`, email, isPrimary) VALUES (61, 'editor@example.com', TRUE)",
        // Everyone reads; the editor may also edit, so read-only below is the view's doing.
        "INSERT INTO Permission (site, target, targetType, actor, actorType, action) VALUES (58, '', 'All', '', 'All', 1)",
        "INSERT INTO Permission (site, target, targetType, actor, actorType, action) VALUES (58, '', 'All', 'editor@example.com', 'Exact', 2)",
        "INSERT INTO Page (site, name, revision, dateTime, `user`, remoteAddress, comment, content) " +
          "VALUES (58, 'Recipe', 1, '2026-09-01 10:00:00', 61, '127.0.0.1', 'first', 'first-revision-marker')",
        "INSERT INTO Page (site, name, revision, dateTime, `user`, remoteAddress, comment, content) " +
          "VALUES (58, 'Recipe', 2, '2026-09-02 10:00:00', 61, '127.0.0.1', 'second', 'second-revision-marker')",
      ).foreach(sql => SQL(sql).execute())
    }
    TestApplication.resetMemoryCaches()
  }

  override def afterAll(): Unit = {
    TestApplication.resetMemoryCaches()
    actorSystem.terminate()
    super.afterAll()
  }

  private def view(query: String): String =
    contentAsString(route(app, FakeRequest(GET, s"/w/Recipe$query")
      .withHeaders(HOST -> host)
      .withSession(SessionLogic.sessionKeySeq -> "61", SessionLogic.sessionKeyNickname -> "editor")).get)

  "an old revision" should {
    "show that revision's content under a notice naming it and the latest" in {
      val html = view("?revision=1")
      Option(System.getenv("AHAWIKI_RENDER_DUMP")).filter(_.nonEmpty).foreach { path =>
        java.nio.file.Files.write(java.nio.file.Paths.get(path), html.getBytes("UTF-8"))
      }
      html must include ("first-revision-marker")
      html must not include "second-revision-marker"
      html must include ("""<div class="oldRevisionNotice" role="note">""")
      html must include ("""This is revision 1, saved <time datetime="2026-09-01T10:00:00">2026-09-01T10:00:00</time> by""")
      html must include ("The latest is revision 2.")
      html must include ("""href="?action=diff&before=1&after=2"""")
    }

    // The editor may write the page, but not from here: a Kanban board drawn from an old
    // revision would save its change onto the latest board and then show that one.
    "be read-only to someone who may write the page" in {
      view("?revision=1") must include ("""data-wiki-writable="false"""")
    }

    // Connected, the next save by anyone redrew a Kanban board as the latest one.
    "not open the page's WebSocket" in {
      view("?revision=1") must include ("if (true) return;")
    }
  }

  "the latest revision" should {
    "carry no notice, stay writable and connect, whether it is asked for by number or not" in {
      Seq("", "?revision=2").foreach { query =>
        val html = view(query)
        html must include ("second-revision-marker")
        html must not include "oldRevisionNotice"
        html must include ("""data-wiki-writable="true"""")
        html must include ("if (false) return;")
      }
    }
  }
}
