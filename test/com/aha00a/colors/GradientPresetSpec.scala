package com.aha00a.colors

import org.scalatest.freespec.AnyFreeSpec

class GradientPresetSpec extends AnyFreeSpec {
  "toHashString" in {
    assert(GradientPreset.greys.getColor(0) === Color(0, 0, 0))
    assert(GradientPreset.greys.getColor(0.5) === Color(127.5, 127.5, 127.5))
    assert(GradientPreset.greys.getColor(1) === Color(255, 255, 255))

    assert(GradientPreset.alpha.getColor(0) === Color(255, 255, 255, 0))
    assert(GradientPreset.alpha.getColor(0.5) === Color(255, 255, 255, 0.5))
    assert(GradientPreset.alpha.getColor(1) === Color(255, 255, 255))

    assert(GradientPreset.ahaWikiMap.getColor(0) === Color(255, 255, 255))
    assert(GradientPreset.ahaWikiMap.reverse().getColor(0) === Color(221, 0, 0))
  }
}
