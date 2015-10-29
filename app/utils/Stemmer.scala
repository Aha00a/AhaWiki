package utils

object Stemmer {
  def stem(s:String):String = {
    val english: String = s.replaceAll("""\W""", "")
    val korean:String = s.replaceAll("""[^가-힣]""", "")
    s
  }
}
