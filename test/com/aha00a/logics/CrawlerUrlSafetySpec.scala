package logics

import com.sun.net.httpserver.{HttpExchange, HttpServer}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.freespec.AnyFreeSpec

import java.io.IOException
import java.net.{InetAddress, InetSocketAddress}
import java.util.concurrent.ConcurrentLinkedQueue
import scala.jdk.CollectionConverters._

/**
 * What the link-preview crawler may fetch, and that it checks every hop of a redirect.
 *
 * Nothing here asks a DNS server: the addresses are literals, and `localhost` comes from the
 * machine's own hosts table. A name that only a resolver can answer would make this spec read
 * differently on a network that answers every name.
 */
class CrawlerUrlSafetySpec extends AnyFreeSpec with BeforeAndAfterAll {

  "validate, which asks no resolver" - {
    "takes http and https with a host" in {
      assert(CrawlerUrlSafety.validate("https://example.com/a?b=c") === Right(()))
      assert(CrawlerUrlSafety.validate("http://example.com") === Right(()))
    }

    "refuses what is not http or https, or has no host" in {
      assert(CrawlerUrlSafety.validate("ftp://example.com/") === Left("Only http/https URLs are allowed"))
      assert(CrawlerUrlSafety.validate("file:///etc/passwd") === Left("Only http/https URLs are allowed"))
      assert(CrawlerUrlSafety.validate("javascript:alert(1)").isLeft)
      assert(CrawlerUrlSafety.validate("not a url").isLeft)
      assert(CrawlerUrlSafety.validate("http:///path").isLeft)
    }

    "refuses an address only the server can reach, written out" in {
      Seq(
        "http://127.0.0.1/", "http://127.0.0.1:10000/w/FrontPage", "http://127.255.255.254/",
        "http://10.1.2.3/", "http://172.16.0.1/", "http://172.31.255.255/", "http://192.168.0.1/",
        "http://169.254.169.254/latest/meta-data/",
        "http://0.0.0.0/", "http://0.1.2.3/", "http://100.64.0.1/", "http://100.127.255.255/",
        "http://192.0.0.1/", "http://198.18.0.1/", "http://198.19.255.255/",
        "http://224.0.0.1/", "http://240.0.0.1/", "http://255.255.255.255/",
        "http://[::1]/", "http://[::]/", "http://[fe80::1]/", "http://[fec0::1]/",
        "http://[fd00:ec2::254]/", "http://[fc00::1]/", "http://[ff02::1]/",
        "http://[::ffff:127.0.0.1]/", "http://[::ffff:169.254.169.254]/", "http://[::127.0.0.1]/",
        "http://[64:ff9b::7f00:1]/", "http://[2002:7f00:1::1]/", "http://[2001:0:7f00:1::1]/",
        "http://user:password@127.0.0.1/", "https://127.0.0.1:8443/",
      ).foreach { url =>
        assert(CrawlerUrlSafety.validate(url) === Left(CrawlerUrlSafety.BlockedHost), url)
      }
    }

    "takes the public addresses that sit beside those ranges" in {
      Seq(
        "http://8.8.8.8/", "http://172.15.255.255/", "http://172.32.0.1/", "http://100.63.255.255/",
        "http://100.128.0.1/", "http://192.0.1.1/", "http://198.17.255.255/", "http://198.20.0.1/",
        "http://169.253.0.1/", "http://169.255.0.1/", "http://223.255.255.254/",
        "http://[2606:4700:4700::1111]/", "http://[2001:4860:4860::8888]/",
      ).foreach { url =>
        assert(CrawlerUrlSafety.validate(url) === Right(()), url)
      }
    }

    "leaves a name, and a number that is not four octets, to the lookup" in {
      assert(CrawlerUrlSafety.validate("http://localhost/") === Right(()))
      assert(CrawlerUrlSafety.validate("http://999.1.1.1/") === Right(()))
    }
  }

  "validateResolved, which looks the host up" - {
    "refuses a name that is this machine" in {
      assert(CrawlerUrlSafety.validateResolved("http://localhost/") === Left(CrawlerUrlSafety.BlockedHost))
      assert(CrawlerUrlSafety.validateResolved("http://localhost:10000/w/FrontPage") === Left(CrawlerUrlSafety.BlockedHost))
    }

    "reads the host as the connection will: percent-decoded, and an internationalised name as ASCII" in {
      // Jsoup turns both of these into localhost before it connects. A check that read them as
      // written would find a name it cannot place and, had it then let them through, fetch localhost.
      assert(CrawlerUrlSafety.validateResolved("http://%6c%6f%63%61%6c%68%6f%73%74/") === Left(CrawlerUrlSafety.BlockedHost))
      assert(CrawlerUrlSafety.validateResolved("http://ⓛⓞⓒⓐⓛⓗⓞⓢⓣ/") === Left(CrawlerUrlSafety.BlockedHost))
      assert(CrawlerUrlSafety.validateResolved("http://%31%32%37.0.0.1/") === Left(CrawlerUrlSafety.BlockedHost))
    }

    "refuses the written-out addresses too" in {
      assert(CrawlerUrlSafety.validateResolved("http://127.0.0.1/") === Left(CrawlerUrlSafety.BlockedHost))
      assert(CrawlerUrlSafety.validateResolved("http://[::1]/") === Left(CrawlerUrlSafety.BlockedHost))
      assert(CrawlerUrlSafety.validateResolved("http://169.254.169.254/") === Left(CrawlerUrlSafety.BlockedHost))
    }

    "takes a public address written out" in {
      assert(CrawlerUrlSafety.validateResolved("http://8.8.8.8/") === Right(()))
    }
  }

  // A server on loopback standing in for the web. The real check refuses loopback, so these tests
  // pass a check of their own: this server's address is "public", and every other host -- the
  // same server reached as `localhost` -- is "inside".
  private val requested = new ConcurrentLinkedQueue[String]()
  private var server: HttpServer = _
  private def base: String = s"http://127.0.0.1:${server.getAddress.getPort}"
  private def inside: String = s"http://localhost:${server.getAddress.getPort}"
  private val onlyTheStandIn: String => Either[String, Unit] = url =>
    if (new java.net.URL(url).getHost == "127.0.0.1") Right(()) else Left(CrawlerUrlSafety.BlockedHost)

  private def answer(exchange: HttpExchange, status: Int, headers: Seq[(String, String)], body: String): Unit = {
    headers.foreach { case (name, value) => exchange.getResponseHeaders.add(name, value) }
    val bytes = body.getBytes("UTF-8")
    if (bytes.isEmpty) exchange.sendResponseHeaders(status, -1)
    else {
      exchange.getResponseHeaders.add("Content-Type", "text/html; charset=utf-8")
      exchange.sendResponseHeaders(status, bytes.length)
      exchange.getResponseBody.write(bytes)
    }
    exchange.close()
  }

  override def beforeAll(): Unit = {
    server = HttpServer.create(new InetSocketAddress(InetAddress.getLoopbackAddress, 0), 0)
    server.createContext("/", (exchange: HttpExchange) => {
      val path = exchange.getRequestURI.getPath
      requested.add(path)
      path match {
        case "/page" => answer(exchange, 200, Seq.empty, "<html><head><title>landed</title></head><body>body</body></html>")
        case "/relative" => answer(exchange, 302, Seq("Location" -> "/page"), "")
        case "/absolute" => answer(exchange, 301, Seq("Location" -> s"$base/relative"), "")
        case "/inward" => answer(exchange, 302, Seq("Location" -> s"$inside/secret"), "")
        case "/secret" => answer(exchange, 200, Seq.empty, "<html><body>only the server should read this</body></html>")
        case "/loop" => answer(exchange, 302, Seq("Location" -> "/loop"), "")
        case "/cookie" => answer(exchange, 302, Seq("Set-Cookie" -> "seen=1; Path=/", "Location" -> "/wants-cookie"), "")
        case "/wants-cookie" =>
          val sent = Option(exchange.getRequestHeaders.getFirst("Cookie")).getOrElse("")
          if (sent.contains("seen=1")) answer(exchange, 200, Seq.empty, "<html><head><title>with cookie</title></head><body></body></html>")
          else answer(exchange, 403, Seq.empty, "<html><body>no cookie</body></html>")
        case _ => answer(exchange, 404, Seq.empty, "<html><body>nothing</body></html>")
      }
    })
    server.start()
  }

  override def afterAll(): Unit = server.stop(0)

  "fetch" - {
    "reads a page" in {
      assert(Crawler.fetch(s"$base/page", onlyTheStandIn).title() === "landed")
    }

    "follows a redirect, relative or absolute, to the page behind it" in {
      assert(Crawler.fetch(s"$base/relative", onlyTheStandIn).title() === "landed")
      assert(Crawler.fetch(s"$base/absolute", onlyTheStandIn).title() === "landed")
    }

    "does not follow a redirect to a host the check refuses, and never asks that host" in {
      requested.clear()
      val refused = intercept[IOException](Crawler.fetch(s"$base/inward", onlyTheStandIn))
      assert(refused.getMessage === CrawlerUrlSafety.BlockedHost)
      assert(requested.asScala.toSeq === Seq("/inward"))
    }

    "does not fetch a first URL the check refuses" in {
      requested.clear()
      val refused = intercept[IOException](Crawler.fetch(s"$inside/secret", onlyTheStandIn))
      assert(refused.getMessage === CrawlerUrlSafety.BlockedHost)
      assert(requested.isEmpty)
    }

    "gives up on a redirect that never ends" in {
      requested.clear()
      val gaveUp = intercept[IOException](Crawler.fetch(s"$base/loop", onlyTheStandIn))
      assert(gaveUp.getMessage === "Too many redirects")
      assert(requested.size === 11)
    }

    "carries a cookie set on a redirect to the next hop" in {
      assert(Crawler.fetch(s"$base/cookie", onlyTheStandIn).title() === "with cookie")
    }

    "with the real check, refuses the stand-in itself: it is on loopback" in {
      requested.clear()
      val refused = intercept[IOException](Crawler.fetch(s"$base/page", CrawlerUrlSafety.validateResolved))
      assert(refused.getMessage === CrawlerUrlSafety.BlockedHost)
      assert(requested.isEmpty)
    }
  }
}
