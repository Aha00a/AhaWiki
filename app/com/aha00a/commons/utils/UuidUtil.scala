package com.aha00a.commons.utils

object UuidUtil {
  def newString: String = java.util.UUID.randomUUID.toString
}
