package logics.wikis

import scala.collection.mutable
import scala.util.matching.Regex

class ExtractConvertInjectVariable {
  val variables: mutable.Map[String, String] = mutable.Map.empty

  def extract(content: String): String = content

  def applyVariables(content: String): String = {
    """\{\{([^}]+)\}\}""".r.replaceAllIn(content, m => {
      val key = m.group(1)
      Regex.quoteReplacement(variables.getOrElse(key, s"{{$key}}"))
    })
  }
}
