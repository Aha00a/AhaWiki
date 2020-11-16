package com.aha00a.commons

import com.aha00a.commons.Implicits._
import org.scalatest.freespec.AnyFreeSpec

class ImplicitsSpec extends AnyFreeSpec {
  "RichBoolean" - {
    "to01" in {
      assert(false.to01 === "0")
      assert(true.to01 === "1")
    }

    "toYN" in {
      assert(true.toYN === "Y")
      assert(false.toYN === "N")
    }
  }

  "RichString" - {
    "toIntOrZero" in {
      assert("aa".toIntOrZero === 0)
      assert("10".toIntOrZero === 10)
    }

    "toDoubleOrZero" in {
      assert("aa".toDoubleOrZero === 0)
      assert("10".toDoubleOrZero === 10)
    }

    "toBoolGenerously" in {
      val nullString: String = null

      assert(!nullString.toBoolGenerously)

      assert(!"".toBoolGenerously)
      assert(!"0".toBoolGenerously)
      assert(!"2".toBoolGenerously)
      assert(!"10".toBoolGenerously)
      assert(!"Test".toBoolGenerously)

      assert("1".toBoolGenerously)
      assert("t".toBoolGenerously)
      assert("T".toBoolGenerously)
      assert("y".toBoolGenerously)
      assert("Y".toBoolGenerously)
      assert("true".toBoolGenerously)
      assert("True".toBoolGenerously)
      assert("TRUE".toBoolGenerously)

    }
  }
}
