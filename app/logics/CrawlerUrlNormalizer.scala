package logics

object CrawlerUrlNormalizer {
  def normalize(rawUrl: String): String = {
    val trimmed = rawUrl.trim
    trimmed.takeWhile(_ != '#')
  }
}
