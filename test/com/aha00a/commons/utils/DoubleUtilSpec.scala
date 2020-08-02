package com.aha00a.commons.utils

import org.scalatest.freespec.AnyFreeSpec

class DoubleUtilSpec extends AnyFreeSpec {
  "lerp" in  {
    assert(DoubleUtil.lerp(0, 100, 0.5) === 50)
    assert(DoubleUtil.lerp(100, 200, 0.5) === 150)
    assert(DoubleUtil.lerp(1000, 2000, 0.5) === 1500)
  }
}
