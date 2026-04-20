package com.aha00a.commons.utils

import org.scalatest.freespec.AnyFreeSpec

class IpAddressUtilSpec extends AnyFreeSpec {
  "mask" - {
    "masks first and second segments for IPv4" in {
      assert(IpAddressUtil.mask("123.45.67.89") === "♡.♡.67.89")
    }

    "masks first five hextets for IPv6" in {
      assert(IpAddressUtil.mask("2001:db8:85a3::8a2e:370:7334") === "♡:♡:♡:♡:♡:8a2e:0370:7334")
    }
  }
}
