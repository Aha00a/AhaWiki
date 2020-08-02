package com.aha00a.colors

import org.scalatest.freespec.AnyFreeSpec

class ColorSpec extends AnyFreeSpec {
  "*" in {
    assert(Color(10, 10, 10) * 0.5 === Color(5, 5, 5))
  }
  
  "/" in {
    assert(Color(10, 10, 10) / 0.5 === Color(20, 20, 20))
  }

  "toHashString" in {
    assert(Color(0, 0, 0).toHashString === "#000000")
    assert(Color(255, 255, 255).toHashString === "#ffffff")
  }
}
