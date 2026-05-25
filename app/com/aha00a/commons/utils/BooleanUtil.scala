package com.aha00a.commons.utils

object BooleanUtil {
  def random(probability: Double = 0.5): Boolean = {
    if (probability <= 0) return false
    if (probability >= 1) return true

    scala.util.Random.nextDouble() < probability
  }
}
