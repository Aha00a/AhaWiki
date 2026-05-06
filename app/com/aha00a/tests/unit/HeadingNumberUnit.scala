package com.aha00a.tests.unit

import com.aha00a.tests.TestUtil
import logics.wikis.HeadingNumber

object HeadingNumberUnit {
  def run(testUtil: TestUtil): Unit = {
    import testUtil.assertEquals

    val headingNumber: HeadingNumber = new HeadingNumber()
    assertEquals(headingNumber.incrGet(1), "1.")
    assertEquals(headingNumber.incrGet(1), "2.")
    assertEquals(headingNumber.incrGet(1), "3.")
    assertEquals(headingNumber.incrGet(1), "4.")
    assertEquals(headingNumber.incrGet(2), "4.1.")
    assertEquals(headingNumber.incrGet(2), "4.2.")
    assertEquals(headingNumber.incrGet(2), "4.3.")
    assertEquals(headingNumber.incrGet(1), "5.")
    assertEquals(headingNumber.incrGet(2), "5.1.")
    assertEquals(headingNumber.incrGet(2), "5.2.")
    assertEquals(headingNumber.incrGet(2), "5.3.")
    assertEquals(headingNumber.incrGet(3), "5.3.1.")
    assertEquals(headingNumber.incrGet(3), "5.3.2.")
    assertEquals(headingNumber.incrGet(3), "5.3.3.")
    assertEquals(headingNumber.incrGet(4), "5.3.3.1.")
    assertEquals(headingNumber.incrGet(4), "5.3.3.2.")
    assertEquals(headingNumber.incrGet(4), "5.3.3.3.")
  }
}
