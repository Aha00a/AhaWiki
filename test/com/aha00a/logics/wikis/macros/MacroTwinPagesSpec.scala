package logics.wikis.macros

import models.tables.Site
import org.scalatest.freespec.AnyFreeSpec

class MacroTwinPagesSpec extends AnyFreeSpec {
  "collectTwinPages" - {
    "keeps only the sites whose copy anonymous users can read" in {
      val currentSite = Site(1, "Current", "CUR", "current.example")
      val publicTwinSite = Site(2, "Public", "PUB", "public.example")
      val privateTwinSite = Site(3, "Private", "PRI", "private.example")

      val result = MacroTwinPages.collectTwinPages(
        pageName = "Foo",
        sitesHavingPage = Seq(currentSite, publicTwinSite, privateTwinSite),
        currentSite = currentSite,
      )(
        anonymousCanRead = site => site.seq == publicTwinSite.seq,
      )

      assert(result.map(twinPage => (twinPage.site.seq, twinPage.pageName)) === Seq((publicTwinSite.seq, "Foo")))
    }

    "skips a site with no mainDomain, which has no address to link to" in {
      val currentSite = Site(1, "Current", "CUR", "current.example")
      val noDomainSite = Site(2, "NoDomain", "ND", "")

      val result = MacroTwinPages.collectTwinPages(
        pageName = "Foo",
        sitesHavingPage = Seq(currentSite, noDomainSite),
        currentSite = currentSite,
      )(
        anonymousCanRead = _ => true,
      )

      assert(result.isEmpty)
    }
  }
}
