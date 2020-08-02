package logics.wikis.macros

import org.scalatest.freespec.AnyFreeSpec

class TraitMacroSpec extends AnyFreeSpec{
  "name" in {
    assert(MacroMonths.name === "Months")
    assert(MacroCalendar.name === "Calendar")
    assert(MacroWeekdayName.name === "WeekdayName")
  }
}
