package logics.wikis.interpreters

object InterpreterMath {
  def apply(argument: String, body: String): String = {
    """<div class="mathjax">$___$""" + body + "$___$</div>"
  }
}
