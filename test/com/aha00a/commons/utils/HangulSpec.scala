package com.aha00a.commons.utils

import org.scalatest.freespec.AnyFreeSpec

class HangulSpec extends AnyFreeSpec {
  "constructor" in {
    assert(Hangul('김') === Hangul(0, 20, 16))
    assert(Hangul('아') === Hangul(11, 0, 0))
    assert(Hangul('하') === Hangul(18, 0, 0))
  }
}
