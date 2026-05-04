package com.aha00a.commons.utils

import org.scalatest.freespec.AnyFreeSpec

class NanoIdUtilSpec extends AnyFreeSpec {
  "newId returns id with requested length and configured alphabet" in {
    val id = NanoIdUtil.newId(12)
    assert(id.length == 12)
    assert(id.matches("^[0123456789abcdefghjkmnpqrtuvwxyz]+$"))
  }

  "newId default length is 16" in {
    val id = NanoIdUtil.newId()
    assert(id.length == 16)
  }

  "newId generates distinct values" in {
    val ids = (1 to 1000).map(_ => NanoIdUtil.newId()).toSet
    assert(ids.size == 1000)
  }
}
