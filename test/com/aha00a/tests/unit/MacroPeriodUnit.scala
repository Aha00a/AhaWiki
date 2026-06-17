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
  }
}
