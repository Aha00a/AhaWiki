package com.aha00a.commons.utils

import org.scalatest.freespec.AnyFreeSpec

class UriUtilSpec extends AnyFreeSpec {
  "encodeURIComponent" in  {
    import com.aha00a.commons.utils.UriUtil.encodeURIComponent

    assert(encodeURIComponent("") === "")
    assert(encodeURIComponent("a") === "a")
    assert(encodeURIComponent("1+1") === "1%2B1")
    assert(encodeURIComponent("1 1") === "1%201")
    assert(encodeURIComponent("!") === "!")
    assert(encodeURIComponent("'") === "'")
    assert(encodeURIComponent("(") === "(")
    assert(encodeURIComponent(")") === ")")
    assert(encodeURIComponent("~") === "~")
  }
}
