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

    "toBoolGenerously" in {
      import com.aha00a.commons.Implicits._

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
