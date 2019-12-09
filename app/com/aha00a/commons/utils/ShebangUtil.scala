package com.aha00a.commons.utils

object ShebangUtil {
  def has(s: String):Boolean = s.startsWith("#!")
  def addWhenNotExist(s:String, shebang:String):String = if(has(s.trim)) s else "#!" + shebang + "\n" + s
}
