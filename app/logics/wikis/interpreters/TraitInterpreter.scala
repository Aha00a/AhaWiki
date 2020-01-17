package logics.wikis.interpreters

import models.{Link, WikiContext}

trait TraitInterpreter {
  val name: String = getClass.getSimpleName.replaceAll("^Interpreter", "").replaceAll("""\$$""", "")

  def interpret(content: String)(implicit wikiContext: WikiContext): String

  def extractLink(content: String)(implicit wikiContext: WikiContext): Seq[Link]
}

