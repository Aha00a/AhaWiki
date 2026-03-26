package logics.wikis.macros

import org.scalatest.freespec.AnyFreeSpec
import provider.EmptyContextWikiPage

class MacroUptimeSpec extends AnyFreeSpec with EmptyContextWikiPage {
  "name" in {
    val htmlString = MacroUptime.toHtmlString("")

    assert(MacroUptime.name === "Uptime")
    assert(htmlString.matches("""\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(?:\.\d+)?Z"""))
    assert(MacroUptime.extractLink("") === Seq())
  }
}
