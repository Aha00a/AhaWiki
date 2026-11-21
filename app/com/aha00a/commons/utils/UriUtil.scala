package com.aha00a.commons.utils

object UriUtil {
  /**
   * A path a browser will read as one on the host it is on: it starts with a single slash. Two
   * slashes start another host's address, and some browsers read a slash and a backslash as two
   * slashes. What a redirect is given has to pass this when any part of it came from a request.
   */
  def isLocalPath(path: String): Boolean =
    path.startsWith("/") && !path.startsWith("//") && !path.startsWith("/\\")

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
