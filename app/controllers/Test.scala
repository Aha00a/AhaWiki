package controllers

import javax.inject.{Inject, Singleton}

import akka.actor.ActorSystem
import logics.wikis.interpreters.InterpreterWiki
import logics.wikis.Interpreters
import models.{PageContent, WikiContext}
import play.api.Logger
import play.api.mvc._

@Singleton
class Test @Inject()(system: ActorSystem) extends Controller {
  case class ExceptionEquals[T](actual:T, expect:T) extends Exception(s"\nActual=($actual)\nExpect=($expect)") {
    Logger.error(actual.toString)
    Logger.error(expect.toString)
  }


  def assertEquals[T](actual:T, expect:T) = {
    if(actual == expect) {

    } else {
      throw new ExceptionEquals(actual, expect)
    }
  }


  def assertEquals[T](actual:Seq[T], expect:Seq[T]) = {
    if(actual.isEmpty && expect.isEmpty) {

    }
    else
    {
      if(actual == expect) {

      } else {
        throw new ExceptionEquals(actual, expect)
      }
    }
  }


  def unit = Action {
    testPageContent()
    testInterpreterTable()
    testInterpreterWiki()
    Ok("Ok.")
  }


  def testPageContent() = {
    {
      val pageContent: PageContent = new PageContent(
        """#!read all
          |#!write aha00a
          |#!redirect FrontPage""".stripMargin)
      assertEquals(pageContent.read, Some("all"))
      assertEquals(pageContent.write, Some("aha00a"))
      assertEquals(pageContent.redirect, Some("FrontPage"))
      assertEquals(pageContent.shebang, Array[String]().toSeq)
      assertEquals(pageContent.content, "")
    }
    {
      val pageContent: PageContent = new PageContent(
        """#!read login
          |#!write login
          |something""".stripMargin)
      assertEquals(pageContent.read, Some("login"))
      assertEquals(pageContent.write, Some("login"))
      assertEquals(pageContent.redirect, None)
      assertEquals(pageContent.shebang, List())
      assertEquals(pageContent.interpreter, None)
      assertEquals(pageContent.content, "something")
    }
    {
      val pageContent: PageContent = new PageContent(
        """#!Wiki
          |#!Html
          |something""".stripMargin)
      assertEquals(pageContent.read, None)
      assertEquals(pageContent.write, None)
      assertEquals(pageContent.redirect, None)
      assertEquals(pageContent.shebang, List("Wiki", "Html"))
      assertEquals(pageContent.content, "something")
    }
    {
      val pageContent: PageContent = new PageContent(
        """#!Paper a b
          |#!read login
          |#!write login
          |something""".stripMargin)
      assertEquals(pageContent.read, Some("login"))
      assertEquals(pageContent.write, Some("login"))
      assertEquals(pageContent.redirect, None)
      assertEquals(pageContent.interpreter, Some("Paper"))
      assertEquals(pageContent.content, "something")
    }
    {
      val pageContent: PageContent = new PageContent(
        """#!read login
          |#!write login
          |#!Paper a b
          |something""".stripMargin)
      assertEquals(pageContent.read, Some("login"))
      assertEquals(pageContent.write, Some("login"))
      assertEquals(pageContent.redirect, None)
      assertEquals(pageContent.interpreter, Some("Paper"))
      assertEquals(pageContent.content, "something")
    }
    {
      val pageContent: PageContent = new PageContent(
        """#!WikiSyntaxPreview Text
          |some text
          |""".stripMargin)
      assertEquals(pageContent.read, None)
      assertEquals(pageContent.write, None)
      assertEquals(pageContent.redirect, None)
      assertEquals(pageContent.shebang, List("WikiSyntaxPreview", "Text"))
      assertEquals(pageContent.content, "some text")
    }
    {
      val pageContent: PageContent = new PageContent(
        """#!WikiSyntaxPreview Html
          |<h1>h1</h1>
          |<p>paragraph</p>
          |""".stripMargin)
      assertEquals(pageContent.read, None)
      assertEquals(pageContent.write, None)
      assertEquals(pageContent.redirect, None)
      assertEquals(pageContent.shebang, List("WikiSyntaxPreview", "Html"))
      assertEquals(pageContent.content, "<h1>h1</h1>\n<p>paragraph</p>")
    }
    {
      val pageContent: PageContent = new PageContent(
        """#!WikiSyntaxPreview Vim
          |#!java
          |class C {
          |    private String s = "Hello, java!";
          |}
          |""".stripMargin)
      assertEquals(pageContent.read, None)
      assertEquals(pageContent.write, None)
      assertEquals(pageContent.redirect, None)
      assertEquals(pageContent.shebang, List("WikiSyntaxPreview", "Vim", "java"))
      assertEquals(pageContent.content, "class C {\n    private String s = \"Hello, java!\";\n}")
    }

  }


  def testInterpreterTable(): Unit = {
    implicit val wikiContext = WikiContext("")(null)

    assertEquals(Interpreters("#!table tsv\na\tb"), <table class="simpleTable"><tr><td>a</td><td>b</td></tr></table>.toString())
    assertEquals(Interpreters("#!table\n#!tsv\na\tb"), <table class="simpleTable"><tr><td>a</td><td>b</td></tr></table>.toString())
    assertEquals(Interpreters("#!table tsv 1\na\tb"), <table class="simpleTable"><tr><th>a</th><th>b</th></tr></table>.toString())
    assertEquals(Interpreters("#!table tsv 0 1\na\tb"), <table class="simpleTable"><tr><th>a</th><td>b</td></tr></table>.toString())
  }


  def testInterpreterWiki(): Unit = {
    implicit val wikiContext = WikiContext("")(null)
    val iw: InterpreterWiki = new InterpreterWiki()

    assertEquals(iw.formatInline("""http://a.com"""), """<a href="http://a.com" target="_blank">http://a.com</a>""")
    assertEquals(iw.formatInline("""http://a.com$"""), """<a href="http://a.com$" target="_blank">http://a.com$</a>""")
    assertEquals(iw.formatInline("""[http://a.com]"""), """<a href="http://a.com" target="_blank">http://a.com</a>""")
    assertEquals(iw.formatInline("""[http://a.com a com]"""), """<a href="http://a.com" target="_blank">a com</a>""")
    assertEquals(iw.formatInline("""[Page]"""), """<a href="Page">Page</a>""")
    assertEquals(iw.formatInline("""[Page Alias]"""), """<a href="Page">Alias</a>""")
    assertEquals(iw.formatInline("""[wiki:Page]"""), """<a href="Page">Page</a>""")
    assertEquals(iw.formatInline("""[wiki:Page Alias]"""), """<a href="Page">Alias</a>""")
    assertEquals(iw.formatInline("""http://a.com/$   [http://a.com]  [http://a.com a com]"""), """<a href="http://a.com/$" target="_blank">http://a.com/$</a>   <a href="http://a.com" target="_blank">http://a.com</a>  <a href="http://a.com" target="_blank">a com</a>""")

    assertEquals(iw.formatInline("""\http://a.com"""), "http://a.com")
    assertEquals(iw.formatInline("""\http://a.com$"""), "http://a.com$")
    assertEquals(iw.formatInline("""\[http://a.com]"""), "[http://a.com]")
    assertEquals(iw.formatInline("""\[http://a.com a com]"""), "[http://a.com a com]")
    assertEquals(iw.formatInline("""\[Page]"""), "[Page]")
    assertEquals(iw.formatInline("""\[Page Alias]"""), "[Page Alias]")
    assertEquals(iw.formatInline("""\[wiki:Page]"""), "[wiki:Page]")
    assertEquals(iw.formatInline("""\[wiki:Page Alias]"""), "[wiki:Page Alias]")

    assertEquals(iw.formatInline("""\\http://a.com"""), """\\<a href="http://a.com" target="_blank">http://a.com</a>""")
    assertEquals(iw.formatInline("""\\http://a.com$"""), """\\<a href="http://a.com$" target="_blank">http://a.com$</a>""")
    assertEquals(iw.formatInline("""\\[http://a.com]"""), """\\<a href="http://a.com" target="_blank">http://a.com</a>""")
    assertEquals(iw.formatInline("""\\[http://a.com a com]"""), """\\<a href="http://a.com" target="_blank">a com</a>""")
    assertEquals(iw.formatInline("""\\[Page]"""), """\\<a href="Page">Page</a>""")
    assertEquals(iw.formatInline("""\\[Page Alias]"""), """\\<a href="Page">Alias</a>""")
    assertEquals(iw.formatInline("""\\[wiki:Page]"""), """\\<a href="Page">Page</a>""")
    assertEquals(iw.formatInline("""\\[wiki:Page Alias]"""), """\\<a href="Page">Alias</a>""")
    
    
    
    InterpreterWiki.extractLink("Src", """http://a.com""")
    InterpreterWiki.extractLink("Src", """http://a.com$""")
    InterpreterWiki.extractLink("Src", """[http://a.com]""")
    InterpreterWiki.extractLink("Src", """[http://a.com a com]""")
    InterpreterWiki.extractLink("Src", """[Page]""")
    InterpreterWiki.extractLink("Src", """[Page Alias]""")
    InterpreterWiki.extractLink("Src", """[wiki:Page]""")
    InterpreterWiki.extractLink("Src", """[wiki:Page Alias]""")
    InterpreterWiki.extractLink("Src", """http://a.com/$   [http://a.com]  [http://a.com a com]""")

    InterpreterWiki.extractLink("Src", """\http://a.com""")
    InterpreterWiki.extractLink("Src", """\http://a.com$""")
    InterpreterWiki.extractLink("Src", """\[http://a.com]""")
    InterpreterWiki.extractLink("Src", """\[http://a.com a com]""")
    InterpreterWiki.extractLink("Src", """\[Page]""")
    InterpreterWiki.extractLink("Src", """\[Page Alias]""")
    InterpreterWiki.extractLink("Src", """\[wiki:Page]""")
    InterpreterWiki.extractLink("Src", """\[wiki:Page Alias]""")

    InterpreterWiki.extractLink("Src", """\\http://a.com""")
    InterpreterWiki.extractLink("Src", """\\http://a.com$""")
    InterpreterWiki.extractLink("Src", """\\[http://a.com]""")
    InterpreterWiki.extractLink("Src", """\\[http://a.com a com]""")
    InterpreterWiki.extractLink("Src", """\\[Page]""")
    InterpreterWiki.extractLink("Src", """\\[Page Alias]""")
    InterpreterWiki.extractLink("Src", """\\[wiki:Page]""")
    InterpreterWiki.extractLink("Src", """\\[wiki:Page Alias]""")

  }


}




