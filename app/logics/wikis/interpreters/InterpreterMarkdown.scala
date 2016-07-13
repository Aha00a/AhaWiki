package logics.wikis.interpreters

object InterpreterMarkdown {
  def apply(body: String): String = com.github.rjeschke.txtmark.Processor.process(body)
}
