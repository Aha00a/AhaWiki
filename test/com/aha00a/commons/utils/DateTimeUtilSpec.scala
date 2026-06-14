package com.aha00a.commons.utils

import org.scalatest.freespec.AnyFreeSpec

class DateTimeUtilSpec extends AnyFreeSpec {
  "expand_ymd_to_ymd_ym_y_md" in  {
    assert(DateTimeUtil.expand_ymd_to_ymd_ym_y_md("2026-06-15") === Seq("2026-06-15", "2026-06", "--06-15"))
    assert(DateTimeUtil.expand_ymd_to_ymd_ym_y_md("invalid") === Seq("invalid"))
  }
}
