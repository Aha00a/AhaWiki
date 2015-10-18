package logics.wikis.interpreters

import logics.wikis.Interpreters
import models.{PageContent, WikiContext}

object InterpreterWikiSyntaxPreview {

  def render(pageContent: PageContent)(implicit wikiContext:WikiContext): String = {
    val argument = pageContent.argument.mkString(" ")
    val body = pageContent.content
    if (argument == "") {
      """<table class="wikiSyntax"><tr><td class="raw">""" +
        Interpreters.interpret("#!text\n" + body) +
        """</td><td class="preview">""" +
        Interpreters.interpret("#!wiki\n" + body) +
        """</td></tr></table>"""
    }
    else
    {
      """<table class="wikiSyntax"><tr><td class="raw">""" +
        Interpreters.interpret(s"#!text\n{{{#!$argument\n" + body + "\n}}}") +
        """</td><td class="preview">""" +
        Interpreters.interpret(s"#!$argument\n" + body) +
        """</td></tr></table>"""
    }
  }
}
