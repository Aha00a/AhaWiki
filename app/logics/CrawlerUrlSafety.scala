package logics

import java.net.URI

object CrawlerUrlSafety {
  def validate(url: String): Either[String, Unit] = {
    val uri = try {
      new URI(url)
    } catch {
      case _: Exception =>
        return Left("Invalid URL")
    }

    val scheme = Option(uri.getScheme).map(_.toLowerCase).getOrElse("")
    if (!(scheme == "http" || scheme == "https"))
      return Left("Only http/https URLs are allowed")

    Right(())
  }
}
