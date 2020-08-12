package logics.wikis.macros

import org.scalatest.freespec.AnyFreeSpec

class TraitMacroSpec extends AnyFreeSpec{
  "name" in {
    assert(MacroCalendar.name === "Calendar")
    assert(MacroWeekdayName.name === "WeekdayName")
  }
}
