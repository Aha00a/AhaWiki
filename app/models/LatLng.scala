package models

import scala.util.hashing.MurmurHash3

object LatLng {
  val Empty: LatLng = LatLng(Double.NaN, Double.NaN)

  /** How far [[LatLng.nudged]] moves a point at most, either way on each axis, in degrees: about 2.8 m. */
  val NudgeMax: Double = 0.5 / 20000
}

case class LatLng(lat: Double, lng: Double) {
  /**
   * This point moved a few metres in a direction fixed by `seed` -- the location's name.
   *
   * Places that share an address geocode to the same point, and their markers would sit exactly on
   * top of each other, so each one is moved a little (2019). The move was random until 2026-10-03:
   * every render drew the map differently, the markers shifted on every reload, and no page with a
   * map could be compared by scripts/compare-instances.sh, which sets aside a page that differs
   * from itself. Derived from the name, the move still separates different places at one address,
   * and it is the same every time.
   */
  def nudged(seed: String): LatLng =
    LatLng(lat + offset(s"$seed\u0000lat"), lng + offset(s"$seed\u0000lng"))

  private def offset(key: String): Double =
    ((MurmurHash3.stringHash(key) & 0xFFFF) / 65535.0 - 0.5) * 2 * LatLng.NudgeMax
}
