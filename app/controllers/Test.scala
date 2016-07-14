package controllers

import javax.inject.{Inject, Singleton}

import akka.actor.ActorSystem
import com.aha00a.commons.utils.Stemmer
import logics.wikis.interpreters.InterpreterVim.Parser
import logics.wikis.interpreters.InterpreterWiki
import logics.wikis.macros.MacroBr
import logics.wikis.{HeadingNumber, Interpreters}
import models.{PageContent, WikiContext}
import play.api.Logger
import play.api.cache.CacheApi
import play.api.mvc._

@Singleton
class Test @Inject()(implicit cacheApi: CacheApi, system: ActorSystem) extends Controller {
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


  def unit = Action { implicit request =>
    testPageContent()
    testInterpreterTable()
    testInterpreterWiki()
    testInterpreterVim()
    testHeadingNumber()
    testStemmer()

    val s: String = "aaa\nbbb\nabc\ndef"
    val q = "b"
    assertEquals(s.split("""(\r\n|\n)+""").filter(_.contains(q)).mkString(" ... "), "bbb ... abc")

    implicit val wikiContext = WikiContext("")
    assertEquals(MacroBr(""), "<br/>")
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
    {
      assertEquals("a:b:::c:".split(":", -1).toList, List("a", "b", "", "", "c", ""))
      val pageContent: PageContent = new PageContent(
        """aaa
          |[[[#!Vim java
          |a
          |
          |b
          |
          |c
          |]]]
          |""".stripMargin)
      assertEquals(pageContent.read, None)
      assertEquals(pageContent.write, None)
      assertEquals(pageContent.redirect, None)
      assertEquals(pageContent.shebang, List[String]())
      assertEquals(pageContent.content, "aaa\n[[[#!Vim java\na\n\nb\n\nc\n]]]")
    }

  }


  def testInterpreterTable()(implicit request: Request[Any], cacheApi: CacheApi): Unit = {
    implicit val wikiContext = WikiContext("")

    assertEquals(Interpreters.interpret("#!table tsv\na\tb"), <table class="simpleTable"><tr><td><p>a</p></td><td><p>b</p></td></tr></table>.toString())
    assertEquals(Interpreters.interpret("#!table\n#!tsv\na\tb"), <table class="simpleTable"><tr><td><p>a</p></td><td><p>b</p></td></tr></table>.toString())
    assertEquals(Interpreters.interpret("#!table tsv 1\na\tb"), <table class="simpleTable"><tr><th><p>a</p></th><th><p>b</p></th></tr></table>.toString())
    assertEquals(Interpreters.interpret("#!table tsv 0 1\na\tb"), <table class="simpleTable"><tr><th><p>a</p></th><td><p>b</p></td></tr></table>.toString())
  }


  def testInterpreterWiki()(implicit request: Request[Any], cacheApi: CacheApi): Unit = {
    implicit val wikiContext = WikiContext("")
    val iw: InterpreterWiki = new InterpreterWiki()

    assertEquals(iw.formatInline("""http://a.com"""), """<a href="http://a.com" target="_blank">http://a.com</a>""")
    assertEquals(iw.formatInline("""http://a.com$"""), """<a href="http://a.com$" target="_blank">http://a.com$</a>""")
    assertEquals(iw.formatInline("""[http://a.com]"""), """<a href="http://a.com" target="_blank">http://a.com</a>""")
    assertEquals(iw.formatInline("""[http://a.com a com]"""), """<a href="http://a.com" target="_blank">a com</a>""")
    assertEquals(iw.formatInline("""[FrontPage]"""), """<a href="FrontPage">FrontPage</a>""")
    assertEquals(iw.formatInline("""[FrontPage Alias]"""), """<a href="FrontPage">Alias</a>""")
    assertEquals(iw.formatInline("""[wiki:FrontPage]"""), """<a href="FrontPage">FrontPage</a>""")
    assertEquals(iw.formatInline("""[wiki:FrontPage Alias]"""), """<a href="FrontPage">Alias</a>""")
    assertEquals(iw.formatInline("""http://a.com/$   [http://a.com]  [http://a.com a com]"""), """<a href="http://a.com/$" target="_blank">http://a.com/$</a>   <a href="http://a.com" target="_blank">http://a.com</a>  <a href="http://a.com" target="_blank">a com</a>""")

    assertEquals(iw.formatInline("""\http://a.com"""), "http://a.com")
    assertEquals(iw.formatInline("""\http://a.com$"""), "http://a.com$")
    assertEquals(iw.formatInline("""\[http://a.com]"""), "[http://a.com]")
    assertEquals(iw.formatInline("""\[http://a.com a com]"""), "[http://a.com a com]")
    assertEquals(iw.formatInline("""\[FrontPage]"""), "[FrontPage]")
    assertEquals(iw.formatInline("""\[FrontPage Alias]"""), "[FrontPage Alias]")
    assertEquals(iw.formatInline("""\[wiki:FrontPage]"""), "[wiki:FrontPage]")
    assertEquals(iw.formatInline("""\[wiki:FrontPage Alias]"""), "[wiki:FrontPage Alias]")

    assertEquals(iw.formatInline("""\\http://a.com"""), """\\<a href="http://a.com" target="_blank">http://a.com</a>""")
    assertEquals(iw.formatInline("""\\http://a.com$"""), """\\<a href="http://a.com$" target="_blank">http://a.com$</a>""")
    assertEquals(iw.formatInline("""\\[http://a.com]"""), """\\<a href="http://a.com" target="_blank">http://a.com</a>""")
    assertEquals(iw.formatInline("""\\[http://a.com a com]"""), """\\<a href="http://a.com" target="_blank">a com</a>""")
    assertEquals(iw.formatInline("""\\[FrontPage]"""), """\\<a href="FrontPage">FrontPage</a>""")
    assertEquals(iw.formatInline("""\\[FrontPage Alias]"""), """\\<a href="FrontPage">Alias</a>""")
    assertEquals(iw.formatInline("""\\[wiki:FrontPage]"""), """\\<a href="FrontPage">FrontPage</a>""")
    assertEquals(iw.formatInline("""\\[wiki:FrontPage Alias]"""), """\\<a href="FrontPage">Alias</a>""")
    
    
    
    InterpreterWiki.extractLink("Src", """http://a.com""")
    InterpreterWiki.extractLink("Src", """http://a.com$""")
    InterpreterWiki.extractLink("Src", """[http://a.com]""")
    InterpreterWiki.extractLink("Src", """[http://a.com a com]""")
    InterpreterWiki.extractLink("Src", """[FrontPage]""")
    InterpreterWiki.extractLink("Src", """[FrontPage Alias]""")
    InterpreterWiki.extractLink("Src", """[wiki:FrontPage]""")
    InterpreterWiki.extractLink("Src", """[wiki:FrontPage Alias]""")
    InterpreterWiki.extractLink("Src", """http://a.com/$   [http://a.com]  [http://a.com a com]""")

    InterpreterWiki.extractLink("Src", """\http://a.com""")
    InterpreterWiki.extractLink("Src", """\http://a.com$""")
    InterpreterWiki.extractLink("Src", """\[http://a.com]""")
    InterpreterWiki.extractLink("Src", """\[http://a.com a com]""")
    InterpreterWiki.extractLink("Src", """\[FrontPage]""")
    InterpreterWiki.extractLink("Src", """\[FrontPage Alias]""")
    InterpreterWiki.extractLink("Src", """\[wiki:FrontPage]""")
    InterpreterWiki.extractLink("Src", """\[wiki:FrontPage Alias]""")

    InterpreterWiki.extractLink("Src", """\\http://a.com""")
    InterpreterWiki.extractLink("Src", """\\http://a.com$""")
    InterpreterWiki.extractLink("Src", """\\[http://a.com]""")
    InterpreterWiki.extractLink("Src", """\\[http://a.com a com]""")
    InterpreterWiki.extractLink("Src", """\\[FrontPage]""")
    InterpreterWiki.extractLink("Src", """\\[FrontPage Alias]""")
    InterpreterWiki.extractLink("Src", """\\[wiki:FrontPage]""")
    InterpreterWiki.extractLink("Src", """\\[wiki:FrontPage Alias]""")

  }


  //noinspection NameBooleanParameters
  def testInterpreterVim(): Unit = {
    def test(p: Parser, syntax: String, content: String, isError:Boolean) = {
      assertEquals(p.syntax, syntax)
      assertEquals(p.content, content)
      assertEquals(p.isError, isError)
    }

    test(Parser(""), "", "", true)
    test(Parser("#!Vi"), "", "", true)
    test(Parser("#!Vim"), "", "", false)
    test(Parser("#!Vim c"), "c", "", false)
    test(Parser("#!Vim cpp"), "cpp", "", false)
    test(Parser("#!Vim\n"), "", "", false)
    test(Parser("#!Vim cpp\n"), "cpp", "", false)
    test(Parser("#!Vim cpp\nasdf"), "cpp", "asdf", false)
    test(Parser("#!Vim\n#!cpp\nasdf"), "cpp", "asdf", false)
    test(Parser("#!Vim cpp\nasdf\nasdf"), "cpp", "asdf\nasdf", false)
    test(Parser("#!Vim\n#!cpp\nasdf\nasdf"), "cpp", "asdf\nasdf", false)
    test(Parser("#!Vim\n#!sh\n#!/bin/sh\nasdf"), "sh", "#!/bin/sh\nasdf", false)
    test(Parser("#!Vim\n#!sh\n#!/bin/sh\nasdf\na\n\nb\n\nc"), "sh", "#!/bin/sh\nasdf\na\n\nb\n\nc", false)
  }


  def testHeadingNumber(): Unit = {
    val headingNumber: HeadingNumber = new HeadingNumber()
    assertEquals(headingNumber.incrGet(1), "1.")
    assertEquals(headingNumber.incrGet(1), "2.")
    assertEquals(headingNumber.incrGet(1), "3.")
    assertEquals(headingNumber.incrGet(1), "4.")
    assertEquals(headingNumber.incrGet(2), "4.1.")
    assertEquals(headingNumber.incrGet(2), "4.2.")
    assertEquals(headingNumber.incrGet(2), "4.3.")
    assertEquals(headingNumber.incrGet(1), "5.")
    assertEquals(headingNumber.incrGet(2), "5.1.")
    assertEquals(headingNumber.incrGet(2), "5.2.")
    assertEquals(headingNumber.incrGet(2), "5.3.")
    assertEquals(headingNumber.incrGet(3), "5.3.1.")
    assertEquals(headingNumber.incrGet(3), "5.3.2.")
    assertEquals(headingNumber.incrGet(3), "5.3.3.")
    assertEquals(headingNumber.incrGet(4), "5.3.3.1.")
    assertEquals(headingNumber.incrGet(4), "5.3.3.2.")
    assertEquals(headingNumber.incrGet(4), "5.3.3.3.")
    assertEquals(headingNumber.incrGet(1), "6.")
    assertEquals(headingNumber.incrGet(2), "6.1.")
    assertEquals(headingNumber.incrGet(2), "6.2.")
    assertEquals(headingNumber.incrGet(2), "6.3.")
    assertEquals(headingNumber.incrGet(3), "6.3.1.")
    assertEquals(headingNumber.incrGet(3), "6.3.2.")
    assertEquals(headingNumber.incrGet(3), "6.3.3.")
    assertEquals(headingNumber.incrGet(4), "6.3.3.1.")
    assertEquals(headingNumber.incrGet(4), "6.3.3.2.")
    assertEquals(headingNumber.incrGet(4), "6.3.3.3.")
  }

  def testStemmer(): Unit = {
    assertEquals(Stemmer.stem("ABC 가나다"), List("ABC", "가나다"))
    assertEquals(Stemmer.stem("""He likes programming. 그는 프로그래밍을 좋아합니다."""), List("He", "like", "program", "그", "프로그래밍", "좋아하다"))
  }
}




