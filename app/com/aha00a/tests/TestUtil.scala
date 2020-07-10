package com.aha00a.tests


class TestUtil(val out: Any => Unit) {
  case class ExceptionEquals[T](actual: T, expect: T)
    extends Exception(s"\nActual=($actual)\nExpect=($expect)") {
    out(actual)
    out(expect)
  }

  def assertEquals[T](actual: T, expect: T): Unit = {
    if (actual == expect) {

    } else {
      throw ExceptionEquals(actual, expect)
    }
  }

  def assertEquals(actual: String, expect: String): Unit = {
    if (actual == expect) {

    } else if (actual == expect.replace("\r", "")) {

    } else {
      throw ExceptionEquals(actual, expect)
    }
  }

  def assertEquals[T](actual: Seq[T], expect: Seq[T]): Unit = {
    if (actual.isEmpty && expect.isEmpty) {

    }
    else {
      if (actual == expect) {

      } else {
        throw ExceptionEquals(actual, expect)
      }
    }
  }
}
