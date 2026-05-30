package logics.wikis

import scala.collection.mutable
import scala.util.matching.Regex

class ExtractConvertInjectVariable {
  val variables: mutable.Map[String, String] = mutable.Map.empty

  private val regexVariableBlock: Regex = """\[\[\[#!Variable\r?\n([\s\S]*?)\]\]\]""".r

  def extract(content: String): String = {
    if (content == null || (!content.contains("[[[") || !content.contains("]]]"))) return content
    regexVariableBlock.replaceAllIn(content, m => {
      val tsv = m.group(1)
      tsv.split("""\r?\n""").foreach { line =>
        val idx = line.indexOf('\t')
        if (idx > 0) {
          val key = line.substring(0, idx).trim
          val value = line.substring(idx + 1).trim
          if (key.nonEmpty) variables(key) = value
        }
      }
      // Preserve line count so edit-link line numbers stay accurate
      "\n" * m.group(0).count(_ == '\n')
    })
  }

  def applyVariables(content: String): String = {
    """\{\{([^}]+)\}\}""".r.replaceAllIn(content, m => {
      val key = m.group(1)
      Regex.quoteReplacement(variables.getOrElse(key, s"{{$key}}"))
    })
  }
}
