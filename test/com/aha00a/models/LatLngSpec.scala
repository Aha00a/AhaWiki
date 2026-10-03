package com.aha00a.models

import models.LatLng
import org.scalatest.freespec.AnyFreeSpec

/** Map markers are moved a few metres so that places sharing an address do not cover each other.
  * The move has to be the same on every render -- see LatLng.nudged for why.
  */
class LatLngSpec extends AnyFreeSpec {
  private val address = LatLng(37.5495, 126.9157)

  "nudged" - {
    "puts the same place on the same point every time" in {
      assert(address.nudged("Aharise") === address.nudged("Aharise"))
    }

    "puts two places at one address on different points" in {
      val names = Seq("Aharise", "Hapjeong Station", "Cafe", "Cafe 2", "")
      val points = names.map(address.nudged)
      assert(points.distinct.size === names.size)
      assert(!points.contains(address))
    }

    "moves a point a few metres at most" in {
      (1 to 1000).map(i => address.nudged(s"place $i")).foreach { p =>
        assert(math.abs(p.lat - address.lat) <= LatLng.NudgeMax)
        assert(math.abs(p.lng - address.lng) <= LatLng.NudgeMax)
      }
    }
  }
}
