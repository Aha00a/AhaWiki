package com.aha00a.colors

case class Gradient(seq: Seq[GradientPoint]) {
  def getColor(double: Double): Color = {
    val right = seq.indexWhere(gp => gp.index > double)
    val left = right - 1
    val gpRight: GradientPoint = seq.lift(right).getOrElse(seq.last)
    val gpLeft: GradientPoint = seq.lift(left).getOrElse(gpRight)
    val leftInner = double - gpLeft.index
    val rightInner = gpRight.index - double
    if(leftInner + rightInner == 0) {
      gpLeft.color
    } else {
      gpLeft.color.lerp(gpRight.color, leftInner / (leftInner + rightInner))
    }
  }

  def reverse(): Gradient = {
    Gradient(seq.reverse.map(gp => GradientPoint(1 - gp.index, gp.color)))
  }
}
