package utils

object UuidUtil {
  def newString: String = java.util.UUID.randomUUID.toString
}
