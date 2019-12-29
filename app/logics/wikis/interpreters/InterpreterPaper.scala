package logics.wikis.interpreters

import models.WikiContext

object InterpreterPaper {
  def interpret(argument:String, wikiText:String)(implicit wikiContext:WikiContext):String = {
    val arguments = argument.split("""\s+""")
    val wikiPageRenderer = InterpreterWiki

    s"""<div class="paperContent ${arguments(0)}">""" +
      wikiPageRenderer(wikiText).split( """<hr/>""")
        .zipWithIndex
        .map { case (s, index) =>
          s"""<div class="page">
             |  <div class="pageHeader">
             |    <div class="documentId">${arguments(1)}</div>
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
