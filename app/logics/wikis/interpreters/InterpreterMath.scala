package logics.wikis.interpreters

object InterpreterMath {
  def interpret(argument: String, body: String): String = {
    """<div class="mathjax">$$$$""" + body + "$$$$</div>"
  }
}
