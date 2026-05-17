package logics

import java.net.InetAddress
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

    val host = Option(uri.getHost).map(_.trim).getOrElse("")
    if (host.isEmpty)
      return Left("URL host is required")

    val addresses = try {
      InetAddress.getAllByName(host)
    } catch {
      case _: Exception =>
        return Left("Unable to resolve host")
    }

    if (addresses.isEmpty)
      return Left("Unable to resolve host")

    Right()
  }
}
