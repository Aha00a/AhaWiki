package com.aha00a.commons.utils

object WarmCoolColors {
  def warm(v:Double, min:Double, max:Double): String = {
     s"hsl(0, 100%, ${100 - ((v - min) * 20 / (max - min))}%)"
  }
}
