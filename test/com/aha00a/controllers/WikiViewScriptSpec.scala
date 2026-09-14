package com.aha00a.controllers

import anorm.SQL
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

/** What a page view writes into its scripts, where the HTML parser pays no attention to quotes.
  *
  * The view puts the page name into its JSON-LD and into JavaScript strings. The JSON went out as
  * Json.stringify wrote it, which leaves `<` alone, and the parser ends a script element at the
  * first `</script` wherever it stands -- so until 2026-09-15 a page name holding one closed the
  * element and the rest ran as markup. A view of a page that does not exist takes its name from
  * the URL, which made that a link anyone could send. The JavaScript strings had the opposite
  * fault: they were HTML-escaped, so a name with `&` reached the script as `&amp;` and matched
  * nothing.
  *
  * The site has a seq and host of its own, because the memory caches are shared by every spec in
  * the JVM and keyed by site.
  */
class WikiViewScriptSpec extends PlaySpec with GuiceOneAppPerSuite with BeforeAndAfterAll {

  private val dbName = TestApplication.randomDbName("view_script")
  private val host = "view-script.test"

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
        "INSERT INTO Site (seq, name, abbr, mainDomain) VALUES (57, 'ViewScript', 'ViewScript', 'view-script.test')",
        "INSERT INTO SiteDomain (site, domain) VALUES (57, 'view-script.test')",
        "INSERT INTO Permission (site, target, targetType, actor, actorType, action) VALUES (57, '', 'All', '', 'All', 1)",
      ).foreach(sql => SQL(sql).execute())
    }
    TestApplication.resetMemoryCaches()
  }

  override def afterAll(): Unit = {
    TestApplication.resetMemoryCaches()
    super.afterAll()
  }

  private def view(encodedName: String): String =
    contentAsString(route(app, FakeRequest(GET, s"/w/$encodedName").withHeaders(HOST -> host)).get)

  "a page view" should {
    "keep a page name inside its script elements" in {
      view("%3C%2Fscript%3E%3Cb%3Einjected%3C%2Fb%3E") must not include "</script><b>injected</b>"
    }

    "hand its scripts the page name itself, not its HTML escape" in {
      view("Tom%20%26%20Jerry") must include ("pageName: \"Tom \\u0026 Jerry\"")
    }

    "show a missing page's name as text, not run it as markup" in {
      view("%5B%5BHtml(%3Cb%3Einjected%3C%2Fb%3E)%5D%5D") must not include "<b>injected</b>"
    }
  }
}
