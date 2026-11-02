package com.aha00a.logics.wikis

import logics.wikis.PageLogic
import org.scalatest.freespec.AnyFreeSpec

/** The document-frequency cut that keeps a term nearly every page holds out of the term vectors. */
class PageLogicTermCutoffSpec extends AnyFreeSpec {
  "documentFrequencyCutoff" - {
    "is a fifth of the calculated pages, rounded up" in {
      assert(PageLogic.documentFrequencyCutoff(1000) === 200L)
      assert(PageLogic.documentFrequencyCutoff(4659) === 932L)
    }

    "never falls below the floor, so a small wiki keeps every term" in {
      assert(PageLogic.documentFrequencyCutoff(0) === PageLogic.MinDocumentFrequencyCutoff)
      assert(PageLogic.documentFrequencyCutoff(30) === PageLogic.MinDocumentFrequencyCutoff)
      assert(PageLogic.documentFrequencyCutoff(100) === PageLogic.MinDocumentFrequencyCutoff)
    }
  }

  "commonTermsOf" - {
    "counts the page itself on top of the others, and keeps a term at the cutoff" in {
      val others = Map(1L -> 20, 2L -> 19, 3L -> 25)
      // 1: 20 others + itself = 21 > 20, common. 2: 19 + 1 = 20, not. 3: common. 4: nobody else, not.
      assert(PageLogic.commonTermsOf(Seq(1L, 2L, 3L, 4L), others, cutoff = 20) === Seq(1L, 3L))
    }
  }
}
