package com.aha00a.commons.utils

object UriUtil {
  def encodeURIComponent(s: String): String = {
    import java.net.URLEncoder

    URLEncoder.encode(s, "UTF-8")
      .replaceAll("\\+", "%20")
      .replaceAll("%21", "!")
      .replaceAll("%27", "'")
      .replaceAll("%28", "(")
      .replaceAll("%29", ")")
      .replaceAll("%7E", "~")
  }
}
