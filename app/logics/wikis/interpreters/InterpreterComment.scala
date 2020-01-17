package logics.wikis.interpreters

import models.{Link, WikiContext}

object InterpreterComment extends TraitInterpreter {
  override def interpret(content: String)(implicit wikiContext: WikiContext): String = ""

  override def extractLink(content: String)(implicit wikiContext: WikiContext): Seq[Link] = Seq()
}
