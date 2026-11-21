package logics

import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import com.aha00a.commons.Implicits._
import play.api.Logger

import java.io.IOException
import java.net.URL
import scala.annotation.tailrec
import scala.jdk.CollectionConverters._

class Crawler(document: Document) {
  val title: String = selectOg("title")
    .orElse(selectText(".profile_dsc .who")) // http://people.search.naver.com
    .orElse(selectText("#printArea > div.view_con01_box > dl > dt")) // http://www.rokps.or.kr
    .orElse(document.title().toOption)
    .getOrElse("")

  val description: String = selectOg("description")
    .orElse(selectText(".profile_dsc .dsc")) // http://people.search.naver.com
    .orElse(selectText("#printArea.view_bbstable")) // http://www.rokps.or.kr
    .orElse(document.body().text().toOption)
    .getOrElse("")

  val image: String = selectOg("image")
    .orElse(selectAttr(".profile_wrap .thmb_wrap .thmb .thmb_img", "src")) // http://people.search.naver.com
    .orElse(selectAttr("#printArea > div.view_con01_box > div > img", "abs:src")) // http://www.rokps.or.kr
    .orElse(selectAttr("link[rel=shortcut icon]",  "abs:href"))
    .getOrElse("")

  def selectOg(key: String): Option[String] = document
    .select(s"meta[property=og:$key]")
    .attr("content").toOption

  def selectText(selector: String): Option[String] = document
    .select(selector)
    .text()
    .replaceAll("""[\s\xA0]+""", " ")
    .toOption

  def selectAttr(selector: String, attr: String): Option[String] = document
    .select(selector)
    .attr(attr)
    .toOption
}

object Crawler {
  private val MaxRedirects = 10

  def fromUrl(url: String)(implicit logger: Logger): Crawler = {
    logger.info(s"Crawler\t$url")
    new Crawler(fetch(url, CrawlerUrlSafety.validateResolved))
  }

  /**
   * The document at `url`, with every hop on the way to it checked.
   *
   * Jsoup would follow redirects by itself, and then only the first URL is ever checked: a public
   * page that answers "see http://127.0.0.1/" gets the crawler inside all the same. So Jsoup is
   * told not to follow, and each Location is checked like the first URL before it is fetched.
   * The check is here rather than in the callers so that no caller can fetch without it. Cookies
   * set on the way are carried along, as Jsoup carries them when it follows by itself -- some
   * sites set one on a redirect and want it back.
   *
   * `validate` is a parameter so a test can let a loopback server through; the application only
   * ever passes CrawlerUrlSafety.validateResolved. A refusal is an IOException with the reason,
   * which the endpoint answers as its error like any other failed fetch.
   */
  private[logics] def fetch(url: String, validate: String => Either[String, Unit]): Document = {
    @tailrec def hop(url: String, cookies: Map[String, String], redirectsLeft: Int): Document = {
      validate(url).left.foreach(reason => throw new IOException(reason))
      val response = Jsoup.connect(url)
        .userAgent("crawler.aha00a.com")
        .timeout(5000)
        .maxBodySize(1024 * 1024)
        .followRedirects(false)
        .cookies(cookies.asJava)
        .execute()
      val location = Option(response.header("Location")).filter(_ => response.statusCode() / 100 == 3)
      location match {
        case None => response.parse()
        case Some(_) if redirectsLeft <= 0 => throw new IOException("Too many redirects")
        case Some(to) => hop(new URL(new URL(url), to).toExternalForm, cookies ++ response.cookies().asScala, redirectsLeft - 1)
      }
    }
    hop(url, Map.empty, MaxRedirects)
  }

  def fromHtml(html: String) = new Crawler(Jsoup.parse(html))
}
