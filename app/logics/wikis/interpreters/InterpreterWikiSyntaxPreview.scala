package logics.wikis.interpreters

import logics.wikis.Interpreters
import models.{PageContent, WikiContext}

object InterpreterWikiSyntaxPreview {

  def interpret(pageContent: PageContent)(implicit wikiContext:WikiContext): String = {
    val argument = pageContent.argument.mkString(" ")
    val body = pageContent.content
    if (argument == "") {
      s"""<table class="wikiSyntax">
         |    <thead>
         |        <tr>
         |            <th>Raw</th>
         |            <th>Preview</th>
         |        </tr>
         |    </thead>
         |    <tbody>
         |        <tr>
         |            <td class="raw">${Interpreters.interpret("#!text\n" + body)}</td>
         |            <td class="preview">${Interpreters.interpret("#!wiki\n" + body)}</td>
         |        </tr>
         |    </tbody>
         |</table>""".stripMargin
    }
    else
    {
      s"""<table class="wikiSyntax">
         |    <thead>
         |        <tr>
         |            <th>Row</th>
         |            <th>Preview</th>
         |        </tr>
         |    </thead>
         |    <tbody>
         |        <tr>
         |            <td class="raw">${Interpreters.interpret(s"#!text\n[[[#!$argument\n" + body + "\n]]]")}</td>
         |            <td class="preview">${Interpreters.interpret(s"#!$argument\n" + body)}</td>
         |        </tr>
         |    </tbody>
         |</table>""".stripMargin
    }
  }
}
