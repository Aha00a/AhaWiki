package logics.wikis.macros

import org.scalatest.freespec.AnyFreeSpec
import provider.EmptyContextWikiPage

class MacroPeriodSpec extends AnyFreeSpec with EmptyContextWikiPage {
  "YYYY-MM-DD" in {
    val empty = ""
    val s = "2025-05-14"

    assert(MacroPeriod.toHtmlString(empty) === """<div class="error">Argument Error - [[Period()]]</div>""")
//    assert(MacroPeriod.toHtmlString(s) === """<div class="error">Argument Error - [[Period()]]</div>""")
  }
}
