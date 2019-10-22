package com.aha00a.commons.utils

object RegexUtil {
  def escapeDollar(s:String): String = s.replaceAllLiterally("$", """\$""")
}
