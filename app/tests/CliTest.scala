package tests

object CliTest extends App {

  import com.aha00a.tests.TestUtil

  run(new TestUtil(x => println(x)))

  def run(testUtil: TestUtil): Unit = {
    import models.WikiContext
    import testUtil.assertEquals

    //noinspection DuplicatedCode
    def testPageContent(): Unit = {
      import models.PageContent
      {
        val pageContent: PageContent = PageContent(
          """#!read all
            |#!write aha00a
            |#!redirect FrontPage""".stripMargin)
        assertEquals(pageContent.read, Some("all"))
        assertEquals(pageContent.write, Some("aha00a"))
        assertEquals(pageContent.redirect, Some("FrontPage"))
        assertEquals(pageContent.shebang, Seq[String]())
        assertEquals(pageContent.content, "")
      }
      {
        val pageContent: PageContent = PageContent(
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
        val pageContent: PageContent = PageContent(
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
        val pageContent: PageContent = PageContent(
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
        val pageContent: PageContent = PageContent(
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
        val pageContent: PageContent = PageContent(
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
        val pageContent: PageContent = PageContent(
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
        val pageContent: PageContent = PageContent(
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
        val pageContent: PageContent = PageContent(
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

    }; testPageContent()
    

    def testWithWikiContext(): Unit = {
      implicit val wikiContext: WikiContext = WikiContext("UnitTest")(null, null, null, null, null)

      def testMacroBr(): Unit = {
        import logics.wikis.macros.MacroBr
        val empty = ""
        val dummy = "dummy"

        assertEquals(MacroBr.toHtmlString(empty), "<br/>")
        assertEquals(MacroBr.toHtmlString(dummy), "<br/>")
        assertEquals(MacroBr.extractLink(empty), Seq())
        assertEquals(MacroBr.extractLink(dummy), Seq())
      }; testMacroBr()


    }; testWithWikiContext()


    def testParboiled(): Unit = {
      import org.parboiled2._

      import scala.util.Try

      //noinspection TypeAnnotation
      class Calculator(val input: ParserInput) extends Parser {
        def InputLine = rule { Expression ~ EOI }

        def Expression: Rule1[Int] = rule {
          Term ~ zeroOrMore(
            '+' ~ Term ~> ((_: Int) + _)
              | '-' ~ Term ~> ((_: Int) - _))
        }

        def Term = rule {
          Factor ~ zeroOrMore(
            '*' ~ Factor ~> ((_: Int) * _)
              | '/' ~ Factor ~> ((_: Int) / _))
        }

        def Factor = rule { Number | Parens }

        def Parens = rule { '(' ~ Expression ~ ')' }

        def Number = rule { capture(Digits) ~> (_.toInt) }

        def Digits = rule { oneOrMore(CharPredicate.Digit) }
      }

      val triedInt: Try[Int] = new Calculator("1+1").InputLine.run()
      assertEquals(triedInt.isSuccess, true)
      assertEquals(triedInt.get, 2)
    }; testParboiled()
  }

}
