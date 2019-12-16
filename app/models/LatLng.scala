package models

import scala.util.Random

case class LatLng(lat: Double, lng: Double) {
  def noise(): Double = (Random.nextDouble() - 0.5) / 20000
  def latWithNoise(): Double = lat + noise()
  def lngWithNoise(): Double = lng + noise()
}
