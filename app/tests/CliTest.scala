package tests

object CliTest extends App {
  import com.aha00a.tests.TestUtil

  val testUtil: TestUtil = new TestUtil(x => println(x))
  run(testUtil)

  def run(testUtil: TestUtil): Unit = {
    import models.WikiContext
    import testUtil.assertEquals

    def testComAha00aCommonsImlicits(): Unit = {
      import com.aha00a.commons.Implicits._
      assertEquals(0, 0)
      assertEquals("aa".toIntOrZero, 0)
      assertEquals("10".toIntOrZero, 10)
    }; testComAha00aCommonsImlicits()

    def testEnglishConverter(): Unit = {
      import com.aha00a.commons.utils.EnglishCaseConverter
      assertEquals(EnglishCaseConverter.splitPascalCase("Person"), Seq("Person"))
      assertEquals(EnglishCaseConverter.splitPascalCase("FrontPage"), Seq("Front", "Page"))

      assertEquals(EnglishCaseConverter.camelCase2TitleCase("someWordsAreHere"), "Some Words Are Here")
      assertEquals(EnglishCaseConverter.pascalCase2TitleCase("FrontPage"), "Front Page")
      assertEquals(EnglishCaseConverter.pascalCase2TitleCase("TVSeries"), "TV Series")
    }; testEnglishConverter()


    def testTraitMacroName(): Unit = {
      import logics.wikis.macros.MacroCalendar
      import logics.wikis.macros.MacroMonths
      import logics.wikis.macros.MacroWeekdayName

      assertEquals(MacroMonths.name, "Months")
      assertEquals(MacroCalendar.name, "Calendar")
      assertEquals(MacroWeekdayName.name, "WeekdayName")
    }; testTraitMacroName()

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
        assertEquals(pageContent.shebang, Array[String]().toSeq)
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

    def testHeadingNumber(): Unit = {
      import logics.wikis.HeadingNumber
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
    }; testHeadingNumber()

    def testBlame(): Unit = {
      import com.aha00a.commons.Implicits._
      import models.Blame

      //noinspection ZeroIndexToHead
      def testBlame1(): Unit = {
        class MetaData(val revision: Int)
        assertEquals(new Blame().size, 0)

        val blame1 = new Blame().next(new MetaData(1), "A".splitLinesSeq())
        assertEquals(blame1.size, 1)
        assertEquals(blame1.seqBlameLine(0).metaData.revision, 1)
        assertEquals(blame1.seqBlameLine(0).item, "A")

        val blame2 = blame1.next(new MetaData(2), "B".splitLinesSeq())
        assertEquals(blame2.size, 1)
        assertEquals(blame2.seqBlameLine(0).metaData.revision, 2)
        assertEquals(blame2.seqBlameLine(0).item, "B")

        val blame3 = blame2.next(new MetaData(3), "a\nb\nc\nd\ne".splitLinesSeq())
        assertEquals(blame3.size, 5)
        assertEquals(blame3.seqBlameLine(0).metaData.revision, 3)
        assertEquals(blame3.seqBlameLine(0).item, "a")
        assertEquals(blame3.seqBlameLine(1).metaData.revision, 3)
        assertEquals(blame3.seqBlameLine(1).item, "b")
        assertEquals(blame3.seqBlameLine(2).metaData.revision, 3)
        assertEquals(blame3.seqBlameLine(2).item, "c")
        assertEquals(blame3.seqBlameLine(3).metaData.revision, 3)
        assertEquals(blame3.seqBlameLine(3).item, "d")
        assertEquals(blame3.seqBlameLine(4).metaData.revision, 3)
        assertEquals(blame3.seqBlameLine(4).item, "e")

        val blame4 = blame3.next(new MetaData(4), "a\nb\nd\ne".splitLinesSeq())
        assertEquals(blame4.size, 4)
        assertEquals(blame4.seqBlameLine(0).metaData.revision, 3)
        assertEquals(blame4.seqBlameLine(0).item, "a")
        assertEquals(blame4.seqBlameLine(1).metaData.revision, 3)
        assertEquals(blame4.seqBlameLine(1).item, "b")
        assertEquals(blame4.seqBlameLine(2).metaData.revision, 3)
        assertEquals(blame4.seqBlameLine(2).item, "d")
        assertEquals(blame4.seqBlameLine(3).metaData.revision, 3)
        assertEquals(blame4.seqBlameLine(3).item, "e")
      }; testBlame1()

      //noinspection ZeroIndexToHead
      def testBlame2(): Unit = {
        class MetaData(val revision: Int)
        assertEquals(new Blame().size, 0)

        val blame1 = new Blame().next(new MetaData(1), "1\n1\n1\n2\n2\n2\n2\n1\n1\n1".splitLinesSeq())
        val blame2 = blame1.next(new MetaData(2), "1\n1\n2\n2\n1\n1".splitLinesSeq())
        assertEquals(blame2.size, 6)
        assertEquals(blame2.seqBlameLine(0).metaData.revision, 1)
        assertEquals(blame2.seqBlameLine(0).item, "1")
        assertEquals(blame2.seqBlameLine(1).metaData.revision, 1)
        assertEquals(blame2.seqBlameLine(1).item, "1")
        assertEquals(blame2.seqBlameLine(2).metaData.revision, 1)
        assertEquals(blame2.seqBlameLine(2).item, "2")
        assertEquals(blame2.seqBlameLine(3).metaData.revision, 1)
        assertEquals(blame2.seqBlameLine(3).item, "2")
        assertEquals(blame2.seqBlameLine(4).metaData.revision, 1)
        assertEquals(blame2.seqBlameLine(4).item, "1")
        assertEquals(blame2.seqBlameLine(5).metaData.revision, 1)
        assertEquals(blame2.seqBlameLine(5).item, "1")
      }; testBlame2()
    }; testBlame()



    def testWithWikiContext():Unit = {
      implicit val wikiContext: WikiContext = WikiContext("UnitTest")(null, null, null, null, null)

      def testMacroBr(): Unit = {
        import logics.wikis.macros.MacroBr
        val empty = ""
        val dummy = "aaaa"

        assertEquals(MacroBr.toHtmlString(empty), "<br/>")
        assertEquals(MacroBr.toHtmlString(dummy), "<br/>")
        assertEquals(MacroBr.extractLink(empty), Seq())
        assertEquals(MacroBr.extractLink(dummy), Seq())
      }; testMacroBr()


    }; testWithWikiContext()



    def testParboiled(): Unit = {
      import org.parboiled2._

      import scala.util.Try

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
