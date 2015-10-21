package utils

class VariableHolder[T](t:T, changed: (T, T) => Unit = (before:T, after:T) => {}, changing:(T, T) => Boolean = (before:T, after:T) => true) {
  private var variable:T = t
  def set(t:T) = {
    if(changing(variable, t)) {
      var before = variable
      variable = t
      changed(before, variable)
    }
    variable
  }
}
