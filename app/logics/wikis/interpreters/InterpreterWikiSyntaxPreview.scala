package logics.wikis.interpreters

import models.{Link, PageContent, WikiContext}

object InterpreterWikiSyntaxPreview extends TraitInterpreter {

  override def interpret(content: String)(implicit wikiContext: WikiContext): String = {
    val pageContent: PageContent = PageContent(content)
    val argument = pageContent.argument.mkString(" ")
    val body = pageContent.content
    if (argument == "") {
      val raw = Interpreters.interpret("#!text\n" + body)
      val preview = Interpreters.interpret("#!wiki\n" + body)
      render(raw, preview)
    }
    else
    {
      val raw = Interpreters.interpret(s"#!text\n[[[#!$argument\n" + body + "\n]]]")
      val preview = Interpreters.interpret(s"#!$argument\n" + body)
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

  override def extractLink(content: String)(implicit wikiContext: WikiContext): Seq[Link] = {
    val pageContent: PageContent = PageContent(content)
    InterpreterWiki.extractLink(pageContent.content)
  }
}
