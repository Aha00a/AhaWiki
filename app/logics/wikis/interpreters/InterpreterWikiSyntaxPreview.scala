package logics.wikis.interpreters

import logics.wikis.Interpreters
import models.{PageContent, WikiContext}

object InterpreterWikiSyntaxPreview {

  def render(pageContent: PageContent)(implicit wikiContext:WikiContext): String = {
    val argument = pageContent.argument.mkString(" ")
    val body = pageContent.content
    if (argument == "") {
      """<table class="wikiSyntax"><tr><td class="raw">""" +
        Interpreters.apply("#!text\n" + body) +
        """</td><td class="preview">""" +
        Interpreters.apply("#!wiki\n" + body) +
        """</td></tr></table>"""
    }
    else
    {
      """<table class="wikiSyntax"><tr><td class="raw">""" +
        Interpreters.apply(s"#!text\n{{{#!$argument\n" + body + "\n}}}") +
        """</td><td class="preview">""" +
        Interpreters.apply(s"#!$argument\n" + body) +
        """</td></tr></table>"""
    }
  }
}
