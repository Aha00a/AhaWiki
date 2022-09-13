package logics

import java.net.URI

object UrlDetector {
  object YouTube {
    def getId(u: String): Option[String] = {
      try {
        val uri = new URI(u)
        if(uri.getHost != "youtube.com" && uri.getHost != "www.youtube.com" && uri.getHost != "youtu.be")
          return None;

        uri.getPath.split("/").last match {
          case "watch" => uri.getQuery.split("&").map(_.split("=", 2)).find(_ (0) == "v").map(_ (1))
          case id => Some(id)
        }
      } catch {
        case _: Throwable => None
      }
    }
  }
}
