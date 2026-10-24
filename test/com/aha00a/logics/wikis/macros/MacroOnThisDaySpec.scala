package com.aha00a.logics.wikis.macros

import com.aha00a.tests.provider.EmptyContextWikiPage
import logics.wikis.ExtractConvertInjectMacro
import logics.wikis.RenderingMode
import logics.wikis.macros.MacroOnThisDay
import logics.wikis.macros.MacroOnThisDay.Entry
import models.ContextWikiPage
import models.tables.CalculatedSchemaOrg
import org.scalatest.freespec.AnyFreeSpec

import java.time.LocalDate

/**
 * The parts of `OnThisDay` that do not need a database: which day a call is about, and how the
 * rows the database returns become a list. Reading the rows and drawing the links are not covered
 * here — both need the page-name cache.
 */
class MacroOnThisDaySpec extends AnyFreeSpec with EmptyContextWikiPage {
  private def on(name: String, today: LocalDate = LocalDate.of(2026, 10, 24)): ContextWikiPage =
    new ContextWikiPage(Seq(name), RenderingMode.Normal).at(today)

  "which day" - {
    "an explicit --MM-DD is that day" in {
      assert(MacroOnThisDay.monthDay("--10-23")(on("Anything")) === Some("10" -> "23"))
    }

    "an explicit full date is its month and day, the year ignored" in {
      assert(MacroOnThisDay.monthDay("1815-12-10")(on("Anything")) === Some("12" -> "10"))
    }

    "no argument on a --MM-DD page is that page's day" in {
      assert(MacroOnThisDay.monthDay("")(on("--03-01")) === Some("03" -> "01"))
    }

    "no argument on a day page is that page's day, not today" in {
      assert(MacroOnThisDay.monthDay("")(on("2020-01-04")) === Some("01" -> "04"))
    }

    "no argument on any other page is today, zero-padded" in {
      assert(MacroOnThisDay.monthDay("")(on("FrontPage", LocalDate.of(2026, 3, 5))) === Some("03" -> "05"))
    }

    "[[OnThisDay]] without parentheses reads as an empty argument" in {
      assert(MacroOnThisDay.monthDay(null)(on("--03-01")) === Some("03" -> "01"))
    }

    "an impossible month or day is not a day" in {
      assert(MacroOnThisDay.monthDay("--13-01")(on("Anything")) === None)
      assert(MacroOnThisDay.monthDay("--10-32")(on("Anything")) === None)
      assert(MacroOnThisDay.monthDay("October 23")(on("Anything")) === None)
    }

    "a bad argument is an argument error rather than today" in {
      val html = MacroOnThisDay.toHtmlString("not-a-day")(on("Anything"))

      assert(html === """<div class="error">Argument Error - [[OnThisDay(not-a-day)]]</div>""")
    }
  }

  "entries" - {
    def row(page: String, prop: String, value: String, cls: String = "Person") = CalculatedSchemaOrg(page, cls, prop, value)

    "run oldest first, with the year-less ones after the dated ones" in {
      val entries = MacroOnThisDay.toEntries(Seq(
        row("Me", "birthDate", "--10-23"),
        row("Ada", "birthDate", "1815-10-23"),
        row("Old", "deathDate", "1066-10-23"),
      ))

      assert(entries === Seq(
        Entry(Some(1066), "deathDate", "Old"),
        Entry(Some(1815), "birthDate", "Ada"),
        Entry(None, "birthDate", "Me"),
      ))
    }

    "list a page once per property, not once per class" in {
      val entries = MacroOnThisDay.toEntries(Seq(
        row("Ada", "birthDate", "1815-10-23", cls = "Person"),
        row("Ada", "birthDate", "1815-10-23", cls = "Thing"),
      ))

      assert(entries === Seq(Entry(Some(1815), "birthDate", "Ada")))
    }

    "keep a page that has two dates on this day under both" in {
      val entries = MacroOnThisDay.toEntries(Seq(
        row("Festival", "startDate", "2020-10-23", cls = "Event"),
        row("Festival", "endDate", "2020-10-23", cls = "Event"),
      ))

      assert(entries.map(_.prop) === Seq("endDate", "startDate"))
    }

    "ignore a property that is not a date, as the Schema renderer does" in {
      val entries = MacroOnThisDay.toEntries(Seq(row("Ada", "description", "1815-10-23")))

      assert(entries.isEmpty)
    }
  }

  "is registered, so the editor offers it and a page can call it" in {
    assert(ExtractConvertInjectMacro.macroNames.contains("OnThisDay"))
  }
}
