package com.aha00a.commons.utils

object RangeUtil {
  def around(i:Int, distance: Int = 0): Range.Inclusive = (i - distance) to (i + distance)
}
