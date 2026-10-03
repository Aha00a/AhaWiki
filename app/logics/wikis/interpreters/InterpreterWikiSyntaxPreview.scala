package logics.wikis.interpreters

import models.{PageContent, ContextWikiPage}

object InterpreterWikiSyntaxPreview extends TraitInterpreter {

  import models.tables.CalculatedLink

  override def toHtmlString(content: String)(implicit wikiContext: ContextWikiPage): String = {
    val pageContent: PageContent = PageContent(content)
    val argument = pageContent.argument.mkString(" ")
    val body = pageContent.content
    val raw =
      if (argument == "") Interpreters.toHtmlString("#!text\n" + body)
      else Interpreters.toHtmlString(s"#!text\n[[[#!$argument\n" + body + "\n]]]")
    render(raw, Interpreters.toHtmlString(previewSource(pageContent)))
  }

  // What the Preview side draws: the body under the interpreter named after WikiSyntaxPreview, or
  // as wiki when none is named. Its links are collected from the same source -- see toSeqLink.
  private def previewSource(pageContent: PageContent): String = {
    val argument = pageContent.argument.mkString(" ")
    if (argument == "") "#!wiki\n" + pageContent.content
    else s"#!$argument\n" + pageContent.content
  }

  private def render(raw: String, preview: String): String = {
    s"""<table class="wikiTableSimple wikiSyntax">
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

  // The links of what the Preview side draws. Until 2026-10-03 the body was read as wiki text
  // whatever interpreter was named, so a code example shown through Vim gave links: InterpreterVim's
  // JavaScript [...Array(1000).keys()] was stored as a link to a page of that name.
  override def toSeqLink(content: String)(implicit wikiContext: ContextWikiPage): Seq[CalculatedLink] =
    Interpreters.toSeqLink(previewSource(PageContent(content)))
}
