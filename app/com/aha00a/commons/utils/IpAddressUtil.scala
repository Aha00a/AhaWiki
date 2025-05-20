package com.aha00a.commons.utils

import java.net.InetAddress

object IpAddressUtil {


  private def expandIPv6(ip: String): Option[Array[String]] = {
    try {
      val inet = InetAddress.getByName(ip)
      val bytes = inet.getAddress
      if (bytes.length == 16) {
        // IPv6: 16 bytes → 8 hextets (2 bytes each)
        val hextets = bytes.grouped(2).map { arr =>
          "%02x%02x".format(arr(0) & 0xff, arr(1) & 0xff)
        }.toArray
        Some(hextets)
      } else {
        None
      }
    } catch {
      case _: Exception => None
    }
  }

  def mask(ip: String): String = {
    if (ip.contains(".")) {
      val parts = ip.split("\\.")
      if (parts.length == 4) {
        s"${parts(0)}.♡.${parts(2)}.${parts(3)}"
      } else {
        ""
      }
    } else if (ip.contains(":")) {
      expandIPv6(ip) match {
        case Some(hextets) =>
          val masked = Seq.fill(3)("♡") ++ hextets.drop(3)
          masked.mkString(":")
        case None =>
          ""
      }
    } else {
      ""
    }
  }
}
