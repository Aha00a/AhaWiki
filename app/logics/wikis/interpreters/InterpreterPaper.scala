package logics.wikis.interpreters

import models.{PageContent, WikiContext}

object InterpreterPaper extends TraitInterpreter {

  import models.tables.Link

  //noinspection ZeroIndexToHead
  override def toHtmlString(content: String)(implicit wikiContext: WikiContext): String = {
    val pageContent: PageContent = PageContent(content)
    s"""<div class="paperContent ${pageContent.argument(0)}">""" +
      InterpreterWiki.toHtmlString(pageContent.content).split( """<hr/>""")
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

  override def toSeqLink(content: String)(implicit wikiContext: WikiContext): Seq[Link] = {
    val pageContent: PageContent = PageContent(content)
    InterpreterWiki.toSeqLink(pageContent.content)
  }
}
