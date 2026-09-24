package com.aha00a.models

import com.aha00a.tests.TestApplication
import com.aha00a.tests.TestSchema
import models.tables.CalculatedLink
import models.tables.GeocodeCache
import models.tables.Page
import models.tables.Site
import org.scalatest.BeforeAndAfterAll
import org.scalatestplus.play.PlaySpec
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.Application
import play.api.cache.SyncCacheApi
import play.api.db.Database
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder

/**
 * A query that interpolates a Seq into `IN (...)` breaks on an empty Seq: it becomes `IN ()`,
 * which the database rejects as a syntax error rather than answering nothing. The call site that
 * passes the empty Seq is usually not the one that looks wrong, so the guard belongs in the query.
 *
 * Found through a `#!Map` block whose body held no location: the page answered 500 because these
 * two were asked for the addresses of no locations. `Page.selectLastRevision(Seq)` had the same
 * hole, guarded at one call site and nowhere else.
 */
class EmptyInClauseSpec extends PlaySpec with GuiceOneAppPerSuite with BeforeAndAfterAll {

  private val dbName = TestApplication.randomDbName("empty_in")
  private implicit val site: Site = Site(9, "EmptyInWiki", "EmptyInWiki", "empty-in.test")

  override def fakeApplication(): Application =
    GuiceApplicationBuilder()
      .configure(TestApplication.baseConfiguration(dbName))
      .overrides(bind[SyncCacheApi].toInstance(new TestApplication.TestSyncCacheApi))
      .build()

  private def db: Database = app.injector.instanceOf[Database]

  override def beforeAll(): Unit = {
    super.beforeAll()
    db.withConnection { implicit connection =>
      TestSchema.createAll()
    }
  }

  "A query with an IN clause" should {
    "answer nothing for an empty Seq instead of failing" in {
      db.withConnection { implicit connection =>
        GeocodeCache.select(Seq.empty) mustBe Seq.empty
        CalculatedLink.selectDstMaxMinCountWhereSrcIsDatePage(Seq.empty) mustBe Seq.empty
        Page.selectLastRevision(Seq.empty) mustBe Seq.empty
      }
    }

    "still run for a non-empty Seq" in {
      db.withConnection { implicit connection =>
        GeocodeCache.select(Seq("nowhere")) mustBe Seq.empty
        CalculatedLink.selectDstMaxMinCountWhereSrcIsDatePage(Seq("Nothing")) mustBe Seq.empty
        Page.selectLastRevision(Seq("NoSuchPage")) mustBe Seq.empty
      }
    }
  }
}
