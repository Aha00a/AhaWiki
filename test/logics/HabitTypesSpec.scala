package logics

import org.scalatest.freespec.AnyFreeSpec

class HabitTypesSpec extends AnyFreeSpec {
  "HabitTypes" - {
    "contains expected values" in {
      assert(HabitTypes.values == Seq("Sleep", "WakeUp", "Meal", "Smoke"))
      assert(HabitTypes.allowedSet == Set("Sleep", "WakeUp", "Meal", "Smoke"))
    }

    "validates allowed habit types" in {
      assert(HabitTypes.isAllowed("Sleep"))
      assert(HabitTypes.isAllowed("WakeUp"))
      assert(!HabitTypes.isAllowed("Running"))
      assert(!HabitTypes.isAllowed(""))
    }

    "maps uriNormalized to css class" in {
      assert(HabitTypes.cssClassFromUriNormalized("habit:Sleep").contains("habit-sleep"))
      assert(HabitTypes.cssClassFromUriNormalized("habit:WakeUp").contains("habit-wakeup"))
      assert(HabitTypes.cssClassFromUriNormalized("habit:Meal").contains("habit-meal"))
      assert(HabitTypes.cssClassFromUriNormalized("habit:Smoke").contains("habit-smoke"))
      assert(HabitTypes.cssClassFromUriNormalized("habit:Unknown").isEmpty)
      assert(HabitTypes.cssClassFromUriNormalized("wiki:Sleep").isEmpty)
    }
  }
}
