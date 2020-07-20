package com.aha00a.commons.utils

object DoubleUtil {
  def lerp(left: Double, right: Double, ratio: Double): Double = left + (right - left) * ratio
}
