package logics

object HabitTypes {
  val sleep = "Sleep"
  val wakeUp = "WakeUp"
  val meal = "Meal"
  val smoke = "Smoke"

  val values: Seq[String] = Seq(sleep, wakeUp, meal, smoke)
  val allowedSet: Set[String] = values.toSet

  val emojiByType: Map[String, String] = Map(
    sleep -> "😴",
    wakeUp -> "⏰",
    meal -> "🍽️",
    smoke -> "🚬",
  )

  val cssClassByType: Map[String, String] = Map(
    sleep -> "habit-sleep",
    wakeUp -> "habit-wakeup",
    meal -> "habit-meal",
    smoke -> "habit-smoke",
  )

  def isAllowed(habitType: String): Boolean = allowedSet.contains(habitType)

  def cssClassFromUriNormalized(uriNormalized: String): Option[String] = {
    if (!uriNormalized.startsWith("habit:")) {
      None
    } else {
      val habitType = uriNormalized.stripPrefix("habit:")
      cssClassByType.get(habitType)
    }
  }
}
