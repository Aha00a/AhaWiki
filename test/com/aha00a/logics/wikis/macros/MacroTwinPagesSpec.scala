package logics.wikis.macros

import models.tables.Site
import org.scalatest.freespec.AnyFreeSpec

class MacroTwinPagesSpec extends AnyFreeSpec {
  "collectTwinPages" - {
    "keeps only same-name pages from other sites that anonymous users can read" in {
      val currentSite = Site(1, "Current", "CUR", "current.example")
      val publicTwinSite = Site(2, "Public", "PUB", "public.example")
      val privateTwinSite = Site(3, "Private", "PRI", "private.example")
      val missingPageSite = Site(4, "Missing", "MIS", "missing.example")

      val result = MacroTwinPages.collectTwinPages(
        pageName = "Foo",
        sites = Seq(currentSite, publicTwinSite, privateTwinSite, missingPageSite),
        currentSite = currentSite,
      )(
        pageExists = site => Set(publicTwinSite.seq, privateTwinSite.seq).contains(site.seq),
        anonymousCanRead = site => site.seq == publicTwinSite.seq,
      )

      assert(result.map(twinPage => (twinPage.site.seq, twinPage.pageName)) === Seq((publicTwinSite.seq, "Foo")))
    }
  }
}
