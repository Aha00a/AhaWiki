package com.aha00a.commons.utils

import org.scalatest.freespec.AnyFreeSpec

class SeqUtilSpec extends AnyFreeSpec {
  "mergeOneByOne" -  {
    import com.aha00a.commons.utils.SeqUtil.mergeOneByOne
    "empty" in {
      assert(mergeOneByOne(Seq()) === Seq())
      assert(mergeOneByOne(Seq(), Seq()) === Seq())
      assert(mergeOneByOne(Seq(), Seq(), Seq()) === Seq())
    }

    "nonEmpty" in {
      assert(mergeOneByOne(Seq(), Seq(1)) === Seq(1))
      assert(mergeOneByOne(Seq(), Seq(1), Seq()) === Seq(1))
      assert(mergeOneByOne(Seq(), Seq(1), Seq(2), Seq()) === Seq(1, 2))
      assert(mergeOneByOne(Seq(), Seq(1, 10), Seq(2, 20), Seq()) === Seq(1, 2, 10, 20))
      assert(mergeOneByOne(Seq(), Seq(1, 2, 3, 4, 5), Seq(10, 20, 30), Seq()) === Seq(1, 10, 2, 20, 3, 30, 4, 5))
    }
  }
}
