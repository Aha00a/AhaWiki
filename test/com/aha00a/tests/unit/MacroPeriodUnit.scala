package com.aha00a.tests.unit

import com.aha00a.commons.utils.LocalDateUtil
import com.aha00a.tests.TestUtil
import logics.wikis.macros.MacroPeriod

import java.time.LocalDate

object MacroPeriodUnit {
  def run(testUtil: TestUtil): Unit = {
    import testUtil.assertEquals

    val sNow = "2025-08-15"
    implicit val localDateNow: LocalDate = LocalDateUtil.tryParse(sNow).getOrElse(LocalDate.now())

    assertEquals(MacroPeriod.doToHtmlString(null), "[[Period()]]")
    assertEquals(MacroPeriod.doToHtmlString(""), "[[Period()]]")

    assertEquals(MacroPeriod.doToHtmlString(sNow), "P0D")

    assertEquals(MacroPeriod.doToHtmlString("2024-08-15"), "P1Y")
    assertEquals(MacroPeriod.doToHtmlString("2026-08-15"), "P1Y")

    assertEquals(MacroPeriod.doToHtmlString(s"$sNow,$sNow"), "P0D")
    assertEquals(MacroPeriod.doToHtmlString(s"$sNow,2024-08-15"), "P1Y")
    assertEquals(MacroPeriod.doToHtmlString(s"$sNow,2026-08-15"), "P1Y")

    assertEquals(MacroPeriod.doToHtmlString("2000-01-01,1999-12-31"), "P1D")

    // A year, or a year and month, is answered at the precision it was written with. Until
    // 2026-11-21 neither could be read at all (a LocalDate needs a day), so the call was printed
    // back as written -- and a #!Schema date of a year alone, `datePublished 1994`, drew
    // `1994 ([[Period(1994)]])`.
    assertEquals(MacroPeriod.doToHtmlString("1994"), "P31Y")
    assertEquals(MacroPeriod.doToHtmlString("2025"), "P0D")
    assertEquals(MacroPeriod.doToHtmlString("2024-03"), "P1Y5M")
    assertEquals(MacroPeriod.doToHtmlString("2025-08"), "P0D")
    // Two dates of different precision meet at the coarser one.
    assertEquals(MacroPeriod.doToHtmlString("2000, 2010-06-30"), "P10Y")
    assertEquals(MacroPeriod.doToHtmlString("2000-01, 2010-06-30"), "P10Y5M")
    // The short forms are read by the shared parser too, as the first of their period.
    assertEquals(LocalDateUtil.tryParse("1994"), Some(LocalDate.of(1994, 1, 1)))
    assertEquals(LocalDateUtil.tryParse("2024-03"), Some(LocalDate.of(2024, 3, 1)))
    assertEquals(com.aha00a.commons.utils.DateTimeUtil.expand_ym_to_ym_y("2026-11"), Seq("2026", "2026-11"))
    assertEquals(MacroPeriod.doToHtmlString("2024-13"), "[[Period(2024-13)]]")
  }
}
