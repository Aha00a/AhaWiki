package com.aha00a.colors

case class Color(r: Double, g: Double, b: Double, a: Double = 1) {

  import com.aha00a.commons.Implicits._

  def lerp(rhs: Color, ratio: Double): Color = {
    import com.aha00a.commons.utils.DoubleUtil
    Color(
      DoubleUtil.lerp(r, rhs.r, ratio),
      DoubleUtil.lerp(g, rhs.g, ratio),
      DoubleUtil.lerp(b, rhs.b, ratio),
      DoubleUtil.lerp(a, rhs.a, ratio)
    )
  }

  def *(double:Double) = Color(r * double, g * double, b * double, a)
  def /(double:Double) = Color(r / double, g / double, b / double, a)

  def toHashString: String = "#" + ((r.toInt << 16) + (g.toInt << 8) + b.toInt).toHexString.padLeft(6, "0")
}
