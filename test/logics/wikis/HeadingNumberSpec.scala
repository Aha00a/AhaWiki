package logics.wikis

import org.scalatest.freespec.AnyFreeSpec

class HeadingNumberSpec extends AnyFreeSpec {
  "incrGet" in {
    val headingNumber: HeadingNumber = new HeadingNumber()
    assert(headingNumber.incrGet(1) === "1.")
    assert(headingNumber.incrGet(1) === "2.")
    assert(headingNumber.incrGet(1) === "3.")
    assert(headingNumber.incrGet(1) === "4.")
    assert(headingNumber.incrGet(2) === "4.1.")
    assert(headingNumber.incrGet(2) === "4.2.")
    assert(headingNumber.incrGet(2) === "4.3.")
    assert(headingNumber.incrGet(1) === "5.")
    assert(headingNumber.incrGet(2) === "5.1.")
    assert(headingNumber.incrGet(2) === "5.2.")
    assert(headingNumber.incrGet(2) === "5.3.")
    assert(headingNumber.incrGet(3) === "5.3.1.")
    assert(headingNumber.incrGet(3) === "5.3.2.")
    assert(headingNumber.incrGet(3) === "5.3.3.")
    assert(headingNumber.incrGet(4) === "5.3.3.1.")
    assert(headingNumber.incrGet(4) === "5.3.3.2.")
    assert(headingNumber.incrGet(4) === "5.3.3.3.")
    assert(headingNumber.incrGet(1) === "6.")
    assert(headingNumber.incrGet(2) === "6.1.")
    assert(headingNumber.incrGet(2) === "6.2.")
    assert(headingNumber.incrGet(2) === "6.3.")
    assert(headingNumber.incrGet(3) === "6.3.1.")
    assert(headingNumber.incrGet(3) === "6.3.2.")
    assert(headingNumber.incrGet(3) === "6.3.3.")
    assert(headingNumber.incrGet(4) === "6.3.3.1.")
    assert(headingNumber.incrGet(4) === "6.3.3.2.")
    assert(headingNumber.incrGet(4) === "6.3.3.3.")
  }
}
