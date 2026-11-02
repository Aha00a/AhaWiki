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
}
