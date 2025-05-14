package logics.wikis.macros

import com.aha00a.commons.utils.LocalDateUtil
import models.ContextWikiPage
import org.scalatest.freespec.AnyFreeSpec
import provider.EmptyContextWikiPage

import java.time.LocalDate

class MacroPeriodSpec extends AnyFreeSpec with EmptyContextWikiPage {
  "YYYY-MM-DD" in {
    val empty = ""
    val sNow = "2025-08-15"

    implicit val contextWikiPage: ContextWikiPage = createContextWikiPage(LocalDateUtil.tryParse(sNow).getOrElse(LocalDate.now()))

    assert(MacroPeriod.toHtmlString(empty) === """<div class="error">Argument Error - [[Period()]]</div>""")

    assert(MacroPeriod.toHtmlString(sNow) === "")

    assert(MacroPeriod.toHtmlString("2024-08-15") === """P1Y ago""")
    assert(MacroPeriod.toHtmlString("2026-08-15") === """P1Y hence""")
  }
}
