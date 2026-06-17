package com.aha00a.tests.unit

import com.aha00a.tests.TestUtil
import logics.UrlDetector

object UrlDetectorUnit {
  def run(testUtil: TestUtil): Unit = {
    import testUtil.assertEquals

    assertEquals(UrlDetector.YouTube.getId(null).isEmpty, true)
    assertEquals(UrlDetector.YouTube.getId("").isEmpty, true)
    assertEquals(UrlDetector.YouTube.getId("https://youtu.be/2wKqfk8pESE").contains("2wKqfk8pESE"), true)
    assertEquals(UrlDetector.YouTube.getId("https://www.youtube.com/watch?feature=youtu.be").isEmpty, true)
  }
}
