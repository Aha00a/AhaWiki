package utils

object RegexUtil {
  def escapeDollar(s:String) = s.replaceAllLiterally("$", """\$""")
}
