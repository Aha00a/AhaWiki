package com.aha00a.commons.utils

object EnglishCaseConverter {
  def camelCase2TitleCase(s :String): String = joinTitleCase(splitCamelCase(s))
  def pascalCase2TitleCase(s :String): String = joinTitleCase(splitPascalCase(s))

  private def splitCamelCase(str: String): Seq[String] = str.replaceAll("""[A-Z]""", " $0").split(" ").toSeq
  private def splitPascalCase(str: String): Seq[String] = str.replaceAll("""[A-Z][a-z]""", " $0").split(" ").toSeq

  private def joinTitleCase(seq: Seq[String]): String = seq.map(s => s.head.toUpper + s.tail.mkString("")).mkString(" ")
}
