package com.aha00a.commons

import org.scalatest.freespec.AnyFreeSpec

class ImplicitsSpec extends AnyFreeSpec {
  "RichString" - {
    "toIntOrZero" in {
      import com.aha00a.commons.Implicits._

      assert("aa".toIntOrZero === 0)
      assert("10".toIntOrZero === 10)
    }

    "toDoubleOrZero" in {
      import com.aha00a.commons.Implicits._

      assert("aa".toDoubleOrZero === 0)
      assert("10".toDoubleOrZero === 10)
    }
  }
}
