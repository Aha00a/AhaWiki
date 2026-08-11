package com.aha00a.logics.wikis.macros

import com.aha00a.tests.provider.EmptyContextWikiPage
import logics.wikis.RenderingMode
import logics.wikis.macros.MacroDayHeader
import logics.wikis.macros.MacroInclude
import models.ContextWikiPage
import org.scalatest.freespec.AnyFreeSpec

/**
 * The half of `DayHeader` that only runs inside an include.
 *
 * It was written in 2020 and stopped being reached in 2025, when a refactor of `IncludeDays`
 * dropped the `push` that put a day page's own name on top of the stack. Nothing failed — the
 * branch simply went quiet.
 *
 * Only the included half is covered here. The standalone half renders date navigation, which
 * needs the page-name cache and therefore a database.
 */
class MacroDayHeaderSpec extends AnyFreeSpec with EmptyContextWikiPage {
  private val monthPage = new ContextWikiPage(Seq("2020-01"), RenderingMode.Normal)
  private val dayPage = monthPage.push("2020-01-04")

  "a day page included into its month page" - {
    "renders a section heading rather than the page heading" in {
      val html = MacroDayHeader.toHtmlString("2020-01-04")(dayPage).trim

      assert(html.startsWith("<h2>2020-01-04 "))
      assert(html.endsWith("</h2>"))
    }

    "names the day it is rendering, not the month that included it" in {
      val html = MacroDayHeader.toHtmlString("")(dayPage)

      assert(html.contains("2020-01-04"))
      assert(!html.contains("<h1>"))
    }
  }

  "argument errors survive the branch split" in {
    val html = MacroDayHeader.toHtmlString("not-a-date")(dayPage)

    assert(html === """<div class="error">Argument Error - [[DayHeader(not-a-date)]]</div>""")
  }

  "Include" - {
    "refuses a page already on the stack" in {
      val circular = monthPage.push("A").push("B")

      val html = MacroInclude.doApply("A", identity)(circular, null)

      assert(html === """<div class="error">Circular Include - [[Include(A)]]</div>""")
    }

    "refuses a page that includes itself" in {
      val html = MacroInclude.doApply("2020-01", identity)(monthPage, null)

      assert(html === """<div class="error">Circular Include - [[Include(2020-01)]]</div>""")
    }
  }
}
