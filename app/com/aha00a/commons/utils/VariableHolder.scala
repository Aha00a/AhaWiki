package com.aha00a.commons.utils

//noinspection ScalaUnusedSymbol
class VariableHolder[T](t:T, changed: (T, T) => Unit = (before:T, after:T) => {}, changing:(T, T) => Boolean = (before:T, after:T) => true) {
  private var variable:T = t
  def :=(t:T): T = {
    if(changing(variable, t)) {
      var before = variable
      variable = t
      changed(before, variable)
    }
    variable
  }
}
