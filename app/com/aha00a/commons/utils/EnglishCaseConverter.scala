package com.aha00a.commons.utils

object EnglishCaseConverter {
  def camelCase2TitleCase(s :String): String = joinTitleCase(splitCamelCase(s))
  
  def splitCamelCase(str: String): Seq[String] = str.replaceAll("""[A-Z]""", " $0").split(" ").toSeq.map(_.toLowerCase())

  def joinTitleCase(seq: Seq[String]): String = seq.map(s => s.head.toUpper + s.tail.mkString("")).mkString(" ")
}
