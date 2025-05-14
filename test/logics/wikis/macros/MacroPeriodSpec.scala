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

    assert(MacroPeriod.toHtmlString(sNow) === "P0D")

    assert(MacroPeriod.toHtmlString("2024-08-15") === "P1Y")
    assert(MacroPeriod.toHtmlString("2026-08-15") === "P1Y")

    assert(MacroPeriod.toHtmlString(s"${sNow},${sNow}") === "P0D")
    assert(MacroPeriod.toHtmlString(s"${sNow},2024-08-15") === "P1Y")
    assert(MacroPeriod.toHtmlString(s"${sNow},2026-08-15") === "P1Y")

    assert(MacroPeriod.toHtmlString(s"2000-01-01,1999-12-31") === "P1D")
  }
}
