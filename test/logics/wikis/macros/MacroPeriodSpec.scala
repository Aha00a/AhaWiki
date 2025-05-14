package logics.wikis.macros

import com.aha00a.commons.utils.LocalDateUtil
import org.scalatest.freespec.AnyFreeSpec

import java.time.LocalDate

class MacroPeriodSpec extends AnyFreeSpec {
  "YYYY-MM-DD" in {
    val sNow = "2025-08-15"

    implicit val localDateNow: LocalDate = LocalDateUtil.tryParse(sNow).getOrElse(LocalDate.now())

    assert(MacroPeriod.doToHtmlString(null) === """[[Period()]]""")
    assert(MacroPeriod.doToHtmlString("") === """[[Period()]]""")

    assert(MacroPeriod.doToHtmlString(sNow) === "P0D")

    assert(MacroPeriod.doToHtmlString("2024-08-15") === "P1Y")
    assert(MacroPeriod.doToHtmlString("2026-08-15") === "P1Y")

    assert(MacroPeriod.doToHtmlString(s"$sNow,$sNow") === "P0D")
    assert(MacroPeriod.doToHtmlString(s"$sNow,2024-08-15") === "P1Y")
    assert(MacroPeriod.doToHtmlString(s"$sNow,2026-08-15") === "P1Y")

    assert(MacroPeriod.doToHtmlString(s"2000-01-01,1999-12-31") === "P1D")
  }
}
