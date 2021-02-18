package com.aha00a.commons.utils

import com.aha00a.commons.Implicits._

object EnglishCaseConverter {
  def camelCase2TitleCase(s :String): String = joinTitleCase(splitCamelCase(s))
  def pascalCase2TitleCase(s :String): String = joinTitleCase(splitPascalCase(s))

  private def splitCamelCase(str: String): Seq[String] = str.replaceAll("""[A-Z]""", " $0").split(" ").toSeq.filter(_.isNotNullOrEmpty)
  def splitPascalCase(str: String): Seq[String] = str.replaceAll("""[A-Z][a-z]""", " $0").split(" ").toSeq.filter(_.isNotNullOrEmpty)

  private def joinTitleCase(seq: Seq[String]): String = seq.map(s => s.head.toUpper.toString + s.tail.mkString("")).mkString(" ")
}
