package logics.wikis.interpreters

object InterpreterMath {
  def apply(argument: String, body: String): String = {
    """<span class="mathjax">$___$""" + body + "$___$</span>"
  }
}
