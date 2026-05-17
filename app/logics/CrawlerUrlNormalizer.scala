package logics

object CrawlerUrlNormalizer {
  def normalize(url: String): String = url.trim.takeWhile(_ != '#')
}
