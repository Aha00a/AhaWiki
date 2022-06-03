package logics.wikis.interpreters

import models.{PageContent, ContextWikiPage}

object InterpreterPaper extends TraitInterpreter {

  import models.tables.Link

  //noinspection ZeroIndexToHead
  override def toHtmlString(content: String)(implicit wikiContext: ContextWikiPage): String = {
    val pageContent: PageContent = PageContent(content)
    s"""<div class="paperContent ${pageContent.argument(0)}">""" +
      InterpreterWiki.toHtmlString(pageContent.content).split( """<hr/>""")
        .zipWithIndex
        .map { case (s, index) =>
          s"""<div class="page">
             |  <!-- ${index} -->
             |  <div class="pageHeader">
             |    <div class="documentId">${pageContent.argument(1)}</div>
             |  </div>
             |  <div class="pageFooter">
             |    <div class="pageNo">${index + 1}</div>
             |  </div>
             |  <div class="pageContent">
             |    <div>
             |      $s
             |    </div>
             |  </div>
             |  <!-- ${index} -->
             |</div>""".stripMargin
        }.mkString("\n") +
    """</div>"""
  }

  override def toSeqLink(content: String)(implicit wikiContext: ContextWikiPage): Seq[Link] = {
    val pageContent: PageContent = PageContent(content)
    InterpreterWiki.toSeqLink(pageContent.content)
  }
}
