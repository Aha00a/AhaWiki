package logics.wikis.interpreters

import models.{PageContent, WikiContext}

object InterpreterPaper extends TraitInterpreter {
  //noinspection ZeroIndexToHead
  override def interpret(content: String)(implicit wikiContext: WikiContext): String = {
    val pageContent: PageContent = PageContent(content)
    s"""<div class="paperContent ${pageContent.argument(0)}">""" +
      InterpreterWiki.interpret(pageContent.content).split( """<hr/>""")
        .zipWithIndex
        .map { case (s, index) =>
          s"""<div class="page">
             |  <div class="pageHeader">
             |    <div class="documentId">${pageContent.argument(1)}</div>
             |  </div>
             |  <div class="pageFooter">
             |    <div class="pageNo">${index + 1}</div>
             |  </div>
             |  <div class="pageContent">
             |    $s
             |  </div>
             |</div>""".stripMargin
        }.mkString("\n") +
    """</div>"""
  }
}
