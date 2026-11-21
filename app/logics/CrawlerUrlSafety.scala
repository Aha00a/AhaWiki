package logics

import java.net.{IDN, Inet4Address, Inet6Address, InetAddress, URL, URLDecoder}
import java.nio.charset.StandardCharsets
import scala.util.Try

/**
 * Which URLs the link-preview crawler may fetch.
 *
 * `GET /api/crawler` needs no login. It fetches the URL the caller names from inside the server's
 * network and answers with the text it found there. So it must not reach what only the server can
 * reach: itself, the private ranges, link-local -- where a cloud host's metadata service answers --
 * and the other ranges in [[isBlocked]].
 *
 * This check was written once before, on 2026-05-18 (099ed192), and taken out nineteen minutes
 * later (80af21bc, 796da147); all three messages say "fix", so the reason is not recorded. For the
 * six months until 2026-11-21 the only test was that the scheme is http or https.
 *
 * Two entry points, because one of them waits on DNS:
 *  - [[validate]] needs no lookup. It runs on the request thread, before the cache is read.
 *  - [[validateResolved]] looks the host up. `Crawler` calls it for every hop of a fetch, on the
 *    crawler's own threads, so a name whose DNS is slow cannot hold a request thread.
 */
object CrawlerUrlSafety {
  val BlockedHost = "Blocked host"

  /** http or https, a host, and not an address literal the crawler may not reach. No lookup. */
  def validate(url: String): Either[String, Unit] = hostOf(url).flatMap { host =>
    literal(host) match {
      case Some(address) if isBlocked(address) => Left(BlockedHost)
      case _ => Right(())
    }
  }

  /**
   * `validate`, and the host looked up: every address the name has must be one the crawler may
   * reach -- a name with one public and one private address is refused. The JVM keeps a lookup
   * for some seconds, so the connection that follows gets the addresses that were checked here.
   */
  def validateResolved(url: String): Either[String, Unit] = hostOf(url).flatMap { host =>
    Try(InetAddress.getAllByName(host).toSeq).toOption.filter(_.nonEmpty) match {
      case None => Left("Unable to resolve host")
      case Some(addresses) if addresses.exists(isBlocked) => Left(BlockedHost)
      case _ => Right(())
    }
  }

  // The host as the connection will read it. Jsoup percent-decodes the host and turns an
  // internationalised name into ASCII before it connects, so `%6c%6f%63%61%6c%68%6f%73%74` and
  // a circled-letter `localhost` both become localhost there. A check that read the URL any other
  // way than the connector does would pass one host and fetch another. java.net.URL, not URI, for
  // the same reason: it is what Jsoup hands the connection.
  private def hostOf(url: String): Either[String, String] =
    Try(new URL(url)).toOption.toRight("Invalid URL").flatMap { parsed =>
      val scheme = parsed.getProtocol.toLowerCase
      if (scheme != "http" && scheme != "https") Left("Only http/https URLs are allowed")
      else Option(parsed.getHost).map(_.trim).filter(_.nonEmpty).toRight("URL host is required").flatMap { written =>
        Try(IDN.toASCII(URLDecoder.decode(written.replace("+", "%2B"), StandardCharsets.UTF_8))).toOption
          .map(_.trim).filter(_.nonEmpty).toRight("Invalid URL")
      }
    }

  // An address written out, read without asking DNS: four decimal numbers, or IPv6 in brackets.
  // Anything else -- `127.1`, a bare number, a name -- is left to validateResolved, which asks the
  // same resolver the connection will.
  private val regexIpv4 = """(\d{1,3})\.(\d{1,3})\.(\d{1,3})\.(\d{1,3})""".r
  private def literal(host: String): Option[InetAddress] = host match {
    case regexIpv4(a, b, c, d) =>
      val octets = Seq(a, b, c, d).map(_.toInt)
      if (octets.forall(_ <= 255)) Some(InetAddress.getByAddress(octets.map(_.toByte).toArray)) else None
    // Bracketed, so the JDK reads it as a literal or refuses it; it never looks a bracketed host up.
    case bracketed if bracketed.startsWith("[") => Try(InetAddress.getByName(bracketed)).toOption
    case _ => None
  }

  /** An address that is not the public web. The JDK already turns `::ffff:a.b.c.d` into a.b.c.d. */
  private[logics] def isBlocked(address: InetAddress): Boolean =
    address.isAnyLocalAddress ||      // 0.0.0.0, ::
      address.isLoopbackAddress ||    // 127.0.0.0/8, ::1
      address.isLinkLocalAddress ||   // 169.254.0.0/16 (a cloud host's metadata service), fe80::/10
      address.isSiteLocalAddress ||   // 10.0.0.0/8, 172.16.0.0/12, 192.168.0.0/16, fec0::/10
      address.isMulticastAddress || (address match {
      case v4: Inet4Address =>
        val b = v4.getAddress.map(_ & 0xff)
        b(0) == 0 ||                                    // 0.0.0.0/8, "this network"
          (b(0) == 100 && (b(1) & 0xc0) == 64) ||       // 100.64.0.0/10, carrier-grade NAT
          (b(0) == 192 && b(1) == 0 && b(2) == 0) ||    // 192.0.0.0/24, protocol assignments
          (b(0) == 198 && (b(1) & 0xfe) == 18) ||       // 198.18.0.0/15, benchmarking
          b(0) >= 240                                   // 240.0.0.0/4, reserved, and broadcast
      case v6: Inet6Address =>
        val b = v6.getAddress.map(_ & 0xff)
        (b(0) & 0xfe) == 0xfc ||                        // fc00::/7, unique local
          v6.isIPv4CompatibleAddress ||                 // ::a.b.c.d
          // The prefixes that carry an IPv4 address inside an IPv6 one, refused whole rather than
          // unwrapped: no site a preview is wanted for is served from them.
          (b(0) == 0x00 && b(1) == 0x64 && b(2) == 0xff && b(3) == 0x9b) ||   // 64:ff9b::/32, NAT64
          (b(0) == 0x20 && b(1) == 0x02) ||                                   // 2002::/16, 6to4
          (b(0) == 0x20 && b(1) == 0x01 && b(2) == 0x00 && b(3) == 0x00)      // 2001::/32, Teredo
      case _ => true
    })
}
