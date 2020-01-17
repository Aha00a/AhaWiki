package logics.wikis.interpreters
import models.WikiContext

object InterpreterComment extends TraitInterpreter {
  override def interpret(content: String)(implicit wikiContext: WikiContext): String = ""
}
