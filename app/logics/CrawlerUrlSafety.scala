package logics

import java.net.{Inet4Address, Inet6Address, InetAddress, URI}

object CrawlerUrlSafety {
  private val AwsMetadataIpv4 = "169.254.169.254"

  def validate(url: String): Either[String, Unit] = {
    val uri = try {
      new URI(url)
    } catch {
      case _: Exception => return Left("Invalid URL")
    }

    val scheme = Option(uri.getScheme).map(_.toLowerCase).getOrElse("")
    if (!(scheme == "http" || scheme == "https")) return Left("Only http/https URLs are allowed")

    val host = Option(uri.getHost).map(_.trim).getOrElse("")
    if (host.isEmpty) return Left("URL host is required")

    val addresses = try {
      InetAddress.getAllByName(host)
    } catch {
      case _: Exception => return Left("Unable to resolve host")
    }

    if (addresses.isEmpty) return Left("Unable to resolve host")

    val blocked = addresses.exists(isBlockedAddress)
    if (blocked) Left("Blocked host") else Right(())
  }

  private def isBlockedAddress(addr: InetAddress): Boolean = {
    if (addr.isAnyLocalAddress || addr.isLoopbackAddress || addr.isLinkLocalAddress || addr.isSiteLocalAddress) {
      true
    } else addr match {
      case ipv4: Inet4Address => isBlockedIpv4(ipv4)
      case ipv6: Inet6Address => isBlockedIpv6(ipv6)
      case _ => false
    }
  }

  private def isBlockedIpv4(ipv4: Inet4Address): Boolean = {
    val bytes = ipv4.getAddress.map(_ & 0xff)
    val ip = ipv4.getHostAddress
    ip == AwsMetadataIpv4 ||
      bytes(0) == 10 ||
      (bytes(0) == 172 && bytes(1) >= 16 && bytes(1) <= 31) ||
      (bytes(0) == 192 && bytes(1) == 168) ||
      bytes(0) == 127 ||
      bytes(0) == 0 ||
      (bytes(0) == 100 && bytes(1) >= 64 && bytes(1) <= 127) ||
      (bytes(0) == 198 && (bytes(1) == 18 || bytes(1) == 19))
  }

  private def isBlockedIpv6(ipv6: Inet6Address): Boolean = {
    ipv6.isLoopbackAddress ||
      ipv6.isLinkLocalAddress ||
      ipv6.isSiteLocalAddress ||
      ipv6.isAnyLocalAddress ||
      ipv6.isIPv4CompatibleAddress ||
      ipv6.getHostAddress.equalsIgnoreCase("::1") ||
      ipv6.getHostAddress.toLowerCase.startsWith("fc") ||
      ipv6.getHostAddress.toLowerCase.startsWith("fd") ||
      ipv6.getHostAddress.toLowerCase.startsWith("fe80:")
  }
}
