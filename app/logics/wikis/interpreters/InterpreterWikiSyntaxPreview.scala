package logics.wikis.interpreters

import models.{PageContent, ContextWikiPage}

object InterpreterWikiSyntaxPreview extends TraitInterpreter {

  import models.tables.Link

  override def toHtmlString(content: String)(implicit wikiContext: ContextWikiPage): String = {
    val pageContent: PageContent = PageContent(content)
    val argument = pageContent.argument.mkString(" ")
    val body = pageContent.content
    if (argument == "") {
      val raw = Interpreters.toHtmlString("#!text\n" + body)
      val preview = Interpreters.toHtmlString("#!wiki\n" + body)
      render(raw, preview)
    }
    else
    {
      val raw = Interpreters.toHtmlString(s"#!text\n[[[#!$argument\n" + body + "\n]]]")
      val preview = Interpreters.toHtmlString(s"#!$argument\n" + body)
      render(raw, preview)
    }
  }
  
  private def render(raw: String, preview: String): String = {
    s"""<table class="wikiSyntax">
       |    <thead>
       |        <tr>
       |            <th>Raw</th>
       |            <th>Preview</th>
       |        </tr>
       |    </thead>
       |    <tbody>
       |        <tr>
       |            <td class="raw">$raw</td>
       |            <td class="preview">$preview</td>
       |        </tr>
       |    </tbody>
       |</table>""".stripMargin
  }

  override def toSeqLink(content: String)(implicit wikiContext: ContextWikiPage): Seq[Link] = {
    val pageContent: PageContent = PageContent(content)
    InterpreterWiki.toSeqLink(pageContent.content)
  }
}
